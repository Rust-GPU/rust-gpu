use crate::config::Config;
use crate::scaffold::compute::{BufferConfig, BufferUsage};
use crate::scaffold::shader::WgpuShader;
use anyhow::Context;
use bytemuck::NoUninit;
use futures::executor::block_on;
use wgpu::{ExperimentalFeatures, util::DeviceExt};

/// More flexible compute test that supports multiple buffers.
pub struct WgpuComputeTest<S> {
    pub features: wgpu::Features,
    pub shader: S,
    pub dispatch: [u32; 3],
    pub buffers: Vec<BufferConfig>,
    pub push_constant: Option<Vec<u8>>,
}

impl<S> WgpuComputeTest<S>
where
    S: WgpuShader,
{
    pub fn new(shader: S, dispatch: [u32; 3], buffers: Vec<BufferConfig>) -> Self {
        Self {
            features: wgpu::Features::empty(),
            shader,
            dispatch,
            buffers,
            push_constant: None,
        }
    }

    pub fn new_with_sizes(shader: S, dispatch: [u32; 3], sizes: &[u64]) -> Self {
        let buffers = sizes
            .iter()
            .map(|&size| BufferConfig::writeback(size as usize))
            .collect();
        Self::new(shader, dispatch, buffers)
    }

    pub fn new_single_buffer(shader: S, dispatch: [u32; 3], size: u64) -> Self {
        Self::new_with_sizes(shader, dispatch, &[size])
    }

    pub fn with_feature(self, feature: wgpu::Features) -> Self {
        Self {
            features: self.features | feature,
            ..self
        }
    }

    pub fn with_immediates<T: NoUninit>(self, data: &T) -> Self {
        Self {
            push_constant: Some(bytemuck::bytes_of(data).to_vec()),
            features: self.features | wgpu::Features::IMMEDIATES,
            ..self
        }
    }

    pub fn run(&self) -> anyhow::Result<Vec<Option<Vec<u8>>>> {
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor::new_without_display_handle());
        let adapter = block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: None,
            force_fallback_adapter: false,
        }))
        .context("Failed to find a suitable GPU adapter")?;
        let (device, queue) = block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("wgpu Device"),
            required_features: wgpu::Features::PASSTHROUGH_SHADERS | self.features,
            required_limits: wgpu::Limits {
                max_immediate_size: 128,
                ..wgpu::Limits::default()
            },
            experimental_features: unsafe { ExperimentalFeatures::enabled() },
            memory_hints: Default::default(),
            trace: Default::default(),
        }))
        .context("Failed to create device")?;

        let buffers = &self.buffers;
        let pipeline = self.shader.create_pipeline(
            &device,
            &device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("Pipeline Layout"),
                bind_group_layouts: &[Some(
                    &device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                        label: Some("Bind Group Layout"),
                        entries: &buffers
                            .iter()
                            .enumerate()
                            .map(|(i, buffer_config)| wgpu::BindGroupLayoutEntry {
                                binding: i as u32,
                                visibility: wgpu::ShaderStages::COMPUTE,
                                ty: wgpu::BindingType::Buffer {
                                    ty: match buffer_config.usage {
                                        BufferUsage::Storage => {
                                            wgpu::BufferBindingType::Storage { read_only: false }
                                        }
                                        BufferUsage::StorageReadOnly => {
                                            wgpu::BufferBindingType::Storage { read_only: true }
                                        }
                                        BufferUsage::Uniform => wgpu::BufferBindingType::Uniform,
                                    },
                                    has_dynamic_offset: false,
                                    min_binding_size: None,
                                },
                                count: None,
                            })
                            .collect::<Vec<_>>(),
                    }),
                )],
                immediate_size: self
                    .push_constant
                    .as_ref()
                    .map(|data| data.len() as u32)
                    .unwrap_or(0),
            }),
        )?;

        // Create buffers.
        let gpu_buffers = self
            .buffers
            .iter()
            .enumerate()
            .map(|(i, buffer_config)| {
                let usage = match buffer_config.usage {
                    BufferUsage::Storage => {
                        wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC
                    }
                    BufferUsage::StorageReadOnly => {
                        wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC
                    }
                    BufferUsage::Uniform => wgpu::BufferUsages::UNIFORM,
                };

                let buffer = if let Some(initial_data) = &buffer_config.initial_data {
                    device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some(&format!("Buffer {i}")),
                        contents: initial_data,
                        usage,
                    })
                } else {
                    device.create_buffer(&wgpu::BufferDescriptor {
                        label: Some(&format!("Buffer {i}")),
                        size: buffer_config.size,
                        usage,
                        mapped_at_creation: false,
                    })
                };

                (buffer_config, buffer)
            })
            .collect::<Vec<_>>();

        // Create bind entries after all buffers are created
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &pipeline.get_bind_group_layout(0),
            entries: &gpu_buffers
                .iter()
                .enumerate()
                .map(|(i, (_, buffer))| wgpu::BindGroupEntry {
                    binding: i as u32,
                    resource: buffer.as_entire_binding(),
                })
                .collect::<Vec<_>>(),
            label: Some("Compute Bind Group"),
        });

        // record command encoder
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Compute Encoder"),
        });
        {
            let mut pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
                label: Some("Compute Pass"),
                timestamp_writes: Default::default(),
            });
            pass.set_pipeline(&pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            if let Some(push_constant) = &self.push_constant {
                pass.set_immediates(0, &push_constant);
            }
            pass.dispatch_workgroups(self.dispatch[0], self.dispatch[1], self.dispatch[2]);
        }

        // Create staging buffers, copy results and initiate mapping.
        let download_buffers = gpu_buffers
            .iter()
            .enumerate()
            .map(|(i, (buffer_config, buffer))| {
                matches!(buffer_config.usage, BufferUsage::Storage).then(|| {
                    let staging_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                        label: Some(&format!("Staging Buffer {i}")),
                        size: buffer_config.size,
                        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
                        mapped_at_creation: false,
                    });
                    encoder.copy_buffer_to_buffer(
                        &buffer,
                        0,
                        &staging_buffer,
                        0,
                        buffer_config.size,
                    );
                    encoder.map_buffer_on_submit(
                        &staging_buffer,
                        wgpu::MapMode::Read,
                        ..,
                        move |r| r.unwrap(),
                    );
                    staging_buffer
                })
            })
            .collect::<Vec<_>>();
        queue.submit(Some(encoder.finish()));
        device.poll(wgpu::PollType::wait_indefinitely())?;

        // copy data from buffers into Vecs
        let results = download_buffers
            .into_iter()
            .map(|buffer| buffer.map(|buffer| buffer.get_mapped_range(..).to_vec()))
            .collect::<Vec<_>>();
        Ok(results)
    }

    pub fn run_test(self, config: &Config) -> anyhow::Result<()> {
        let outputs = self.run()?;
        // Write the first storage buffer output to the file.
        for output in &outputs {
            if let Some(output) = output {
                if !output.is_empty() {
                    config.write_result(output)?;
                    return Ok(());
                }
            }
        }
        anyhow::bail!("No storage buffer output found")
    }
}
