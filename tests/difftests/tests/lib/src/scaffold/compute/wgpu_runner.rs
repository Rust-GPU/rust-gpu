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

    pub fn with_push_constant<T: NoUninit>(self, data: &T) -> Self {
        Self {
            push_constant: Some(bytemuck::bytes_of(data).to_vec()),
            ..self
        }
    }

    pub fn run(self) -> anyhow::Result<Vec<Vec<u8>>> {
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
            #[cfg(target_os = "linux")]
            backends: wgpu::Backends::VULKAN,
            #[cfg(not(target_os = "linux"))]
            backends: wgpu::Backends::PRIMARY,
            flags: Default::default(),
            memory_budget_thresholds: Default::default(),
            backend_options: wgpu::BackendOptions {
                #[cfg(target_os = "windows")]
                dx12: wgpu::Dx12BackendOptions {
                    shader_compiler: wgpu::Dx12Compiler::StaticDxc,
                    ..Default::default()
                },
                ..Default::default()
            },
        });
        let adapter = block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: None,
            force_fallback_adapter: false,
        }))
        .context("Failed to find a suitable GPU adapter")?;
        let (device, queue) = block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("wgpu Device"),
            required_features: wgpu::Features::EXPERIMENTAL_PASSTHROUGH_SHADERS | self.features,
            required_limits: wgpu::Limits {
                max_push_constant_size: 128,
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
                bind_group_layouts: &[&device.create_bind_group_layout(
                    &wgpu::BindGroupLayoutDescriptor {
                        label: Some("Bind Group Layout"),
                        entries: &buffers
                            .iter()
                            .enumerate()
                            .map(|(i, buffer_config)| wgpu::BindGroupLayoutEntry {
                                binding: i as u32,
                                visibility: wgpu::ShaderStages::COMPUTE,
                                ty: match buffer_config.usage {
                                    BufferUsage::Storage => wgpu::BindingType::Buffer {
                                        ty: wgpu::BufferBindingType::Storage { read_only: false },
                                        has_dynamic_offset: false,
                                        min_binding_size: None,
                                    },
                                    BufferUsage::StorageReadOnly => wgpu::BindingType::Buffer {
                                        ty: wgpu::BufferBindingType::Storage { read_only: true },
                                        has_dynamic_offset: false,
                                        min_binding_size: None,
                                    },
                                    BufferUsage::Uniform => wgpu::BindingType::Buffer {
                                        ty: wgpu::BufferBindingType::Uniform,
                                        has_dynamic_offset: false,
                                        min_binding_size: None,
                                    },
                                },
                                count: None,
                            })
                            .collect::<Vec<_>>(),
                    },
                )],
                push_constant_ranges: self
                    .push_constant
                    .as_ref()
                    .map(|data| wgpu::PushConstantRange {
                        stages: wgpu::ShaderStages::COMPUTE,
                        range: 0..data.len() as u32,
                    })
                    .as_slice(),
            }),
        )?;

        // Create buffers.
        let mut gpu_buffers = Vec::new();

        for (i, buffer_config) in self.buffers.iter().enumerate() {
            let usage = match buffer_config.usage {
                BufferUsage::Storage => wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC,
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
                let buffer = device.create_buffer(&wgpu::BufferDescriptor {
                    label: Some(&format!("Buffer {i}")),
                    size: buffer_config.size,
                    usage,
                    mapped_at_creation: true,
                });
                {
                    // Zero the buffer.
                    let initial_data = vec![0u8; buffer_config.size as usize];
                    let mut mapping = buffer.slice(..).get_mapped_range_mut();
                    mapping.copy_from_slice(&initial_data);
                }
                buffer.unmap();
                buffer
            };

            gpu_buffers.push(buffer);
        }

        // Create bind entries after all buffers are created
        let bind_entries: Vec<_> = gpu_buffers
            .iter()
            .enumerate()
            .map(|(i, buffer)| wgpu::BindGroupEntry {
                binding: i as u32,
                resource: buffer.as_entire_binding(),
            })
            .collect();

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &pipeline.get_bind_group_layout(0),
            entries: &bind_entries,
            label: Some("Compute Bind Group"),
        });

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
            if let Some(push_constant) = self.push_constant {
                pass.set_push_constants(0, &push_constant);
            }
            pass.dispatch_workgroups(self.dispatch[0], self.dispatch[1], self.dispatch[2]);
        }

        // Create staging buffers and copy results.
        let mut staging_buffers = Vec::new();
        for (i, buffer_config) in self.buffers.iter().enumerate() {
            if matches!(
                buffer_config.usage,
                BufferUsage::Storage | BufferUsage::StorageReadOnly
            ) {
                let staging_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                    label: Some(&format!("Staging Buffer {i}")),
                    size: buffer_config.size,
                    usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
                    mapped_at_creation: false,
                });
                encoder.copy_buffer_to_buffer(
                    &gpu_buffers[i],
                    0,
                    &staging_buffer,
                    0,
                    buffer_config.size,
                );
                staging_buffers.push(Some(staging_buffer));
            } else {
                staging_buffers.push(None);
            }
        }

        queue.submit(Some(encoder.finish()));

        // Read back results.
        let mut results = Vec::new();
        for staging_buffer in staging_buffers {
            if let Some(buffer) = staging_buffer {
                let buffer_slice = buffer.slice(..);
                let (sender, receiver) = futures::channel::oneshot::channel();
                buffer_slice.map_async(wgpu::MapMode::Read, move |res| {
                    let _ = sender.send(res);
                });
                device.poll(wgpu::PollType::wait_indefinitely())?;
                block_on(receiver)
                    .context("mapping canceled")?
                    .context("mapping failed")?;
                let data = buffer_slice.get_mapped_range().to_vec();
                buffer.unmap();
                results.push(data);
            } else {
                results.push(Vec::new());
            }
        }

        Ok(results)
    }

    pub fn run_test(self, config: &Config) -> anyhow::Result<()> {
        let buffers = self.buffers.clone();
        let outputs = self.run()?;
        // Write the first storage buffer output to the file.
        for (output, buffer_config) in outputs.iter().zip(&buffers) {
            if matches!(buffer_config.usage, BufferUsage::Storage) && !output.is_empty() {
                config.write_result(output)?;
                return Ok(());
            }
        }
        anyhow::bail!("No storage buffer output found")
    }
}
