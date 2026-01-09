use super::backend::{self, ComputeBackend};
use crate::config::Config;
use crate::scaffold::shader::RustComputeShader;
use crate::scaffold::shader::WgpuShader;
use crate::scaffold::shader::WgslComputeShader;
use anyhow::Context;
use bytemuck::Pod;
use futures::executor::block_on;
use std::{borrow::Cow, sync::Arc};
use wgpu::{
    BindGroupLayout, BindGroupLayoutDescriptor, BindGroupLayoutEntry, BindingType,
    BufferBindingType, ExperimentalFeatures, InstanceDescriptor, PipelineCompilationOptions,
    PipelineLayoutDescriptor, ShaderStages, util::DeviceExt,
};

pub type BufferConfig = backend::BufferConfig;
pub type BufferUsage = backend::BufferUsage;

/// Compute test that is generic over the shader type.
pub struct WgpuComputeTest<S> {
    shader: S,
    dispatch: [u32; 3],
    output_bytes: u64,
}

/// More flexible compute test that supports multiple buffers.
pub struct WgpuComputeTestMultiBuffer<S> {
    shader: S,
    dispatch: [u32; 3],
    buffers: Vec<BufferConfig>,
}

/// Compute test that supports push constants.
pub struct WgpuComputeTestPushConstants<S> {
    shader: S,
    dispatch: [u32; 3],
    buffers: Vec<BufferConfig>,
    push_constants_size: u32,
    push_constants_data: Vec<u8>,
}

impl<S> WgpuComputeTest<S>
where
    S: WgpuShader,
{
    pub fn new(shader: S, dispatch: [u32; 3], output_bytes: u64) -> Self {
        Self {
            shader,
            dispatch,
            output_bytes,
        }
    }

    fn init() -> anyhow::Result<(wgpu::Device, wgpu::Queue)> {
        Self::init_with_features(wgpu::Features::empty())
    }

    fn init_with_features(features: wgpu::Features) -> anyhow::Result<(wgpu::Device, wgpu::Queue)> {
        block_on(async {
            let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
                backends: wgpu::Backends::PRIMARY,
                ..InstanceDescriptor::default()
            });
            let adapter = instance
                .request_adapter(&wgpu::RequestAdapterOptions {
                    power_preference: wgpu::PowerPreference::HighPerformance,
                    compatible_surface: None,
                    force_fallback_adapter: false,
                })
                .await
                .context("Failed to find a suitable GPU adapter")?;
            let (device, queue) = adapter
                .request_device(&wgpu::DeviceDescriptor {
                    label: Some("wgpu Device"),
                    required_features: features,
                    required_limits: wgpu::Limits {
                        max_push_constant_size: 128,
                        ..wgpu::Limits::default()
                    },
                    experimental_features: unsafe { ExperimentalFeatures::enabled() },
                    memory_hints: Default::default(),
                    trace: Default::default(),
                })
                .await
                .context("Failed to create device")?;
            Ok((device, queue))
        })
    }

    fn run_internal<I>(self, input: Option<I>) -> anyhow::Result<Vec<u8>>
    where
        I: Sized + Pod,
    {
        let (device, queue) = Self::init()?;
        let (module, entrypoint) = self.shader.create_module(&device)?;
        let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
            label: Some("Compute Pipeline"),
            layout: None,
            module: &module,
            entry_point: entrypoint.as_deref(),
            compilation_options: PipelineCompilationOptions::default(),
            cache: None,
        });

        // Create the output buffer.
        let output_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Output Buffer"),
            size: self.output_bytes,
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC,
            mapped_at_creation: true,
        });
        {
            // Zero the buffer.
            let initial_data = vec![0u8; self.output_bytes as usize];
            let mut mapping = output_buffer.slice(..).get_mapped_range_mut();
            mapping.copy_from_slice(&initial_data);
        }
        output_buffer.unmap();

        // Build the bind group.
        let bind_group = if let Some(input_val) = input {
            let input_bytes = bytemuck::bytes_of(&input_val);
            let input_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("Input Buffer"),
                contents: input_bytes,
                usage: wgpu::BufferUsages::UNIFORM,
            });
            device.create_bind_group(&wgpu::BindGroupDescriptor {
                layout: &pipeline.get_bind_group_layout(0),
                entries: &[
                    // Binding 0: uniform input.
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: input_buffer.as_entire_binding(),
                    },
                    // Binding 1: storage output.
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: output_buffer.as_entire_binding(),
                    },
                ],
                label: Some("Compute Bind Group (with input)"),
            })
        } else {
            device.create_bind_group(&wgpu::BindGroupDescriptor {
                layout: &pipeline.get_bind_group_layout(0),
                entries: &[
                    // Binding 0: storage output.
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: output_buffer.as_entire_binding(),
                    },
                ],
                label: Some("Compute Bind Group (no input)"),
            })
        };

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
            pass.dispatch_workgroups(self.dispatch[0], self.dispatch[1], self.dispatch[2]);
        }

        // Create a staging buffer.
        let staging_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Staging Buffer"),
            size: self.output_bytes,
            usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        encoder.copy_buffer_to_buffer(&output_buffer, 0, &staging_buffer, 0, self.output_bytes);
        queue.submit(Some(encoder.finish()));

        let buffer_slice = staging_buffer.slice(..);
        let (sender, receiver) = futures::channel::oneshot::channel();
        buffer_slice.map_async(wgpu::MapMode::Read, move |res| {
            let _ = sender.send(res);
        });
        device.poll(wgpu::PollType::wait_indefinitely())?;
        block_on(receiver)
            .context("mapping canceled")?
            .context("mapping failed")?;
        let data = buffer_slice.get_mapped_range().to_vec();
        staging_buffer.unmap();
        Ok(data)
    }

    /// Runs the compute shader with no input.
    pub fn run(self) -> anyhow::Result<Vec<u8>> {
        self.run_internal::<()>(None)
    }

    /// Runs the compute shader with provided input.
    pub fn run_with_input<I>(self, input: I) -> anyhow::Result<Vec<u8>>
    where
        I: Sized + Pod,
    {
        self.run_internal(Some(input))
    }

    /// Runs the compute shader with no input and writes the output to a file.
    pub fn run_test(self, config: &Config) -> anyhow::Result<()> {
        let output = self.run()?;
        config.write_result(&output)?;
        Ok(())
    }

    /// Runs the compute shader with provided input and writes the output to a file.
    pub fn run_test_with_input<I>(self, config: &Config, input: I) -> anyhow::Result<()>
    where
        I: Sized + Pod,
    {
        let output = self.run_with_input(input)?;
        config.write_result(&output)?;
        Ok(())
    }
}

/// wgpu backend implementation for the generic `ComputeBackend` trait
pub struct WgpuBackend {
    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
}

impl ComputeBackend for WgpuBackend {
    fn init() -> anyhow::Result<Self> {
        let (device, queue) = WgpuComputeTest::<RustComputeShader>::init()?;
        Ok(Self {
            device: Arc::new(device),
            queue: Arc::new(queue),
        })
    }

    fn run_compute(
        &self,
        spirv_words: &[u32],
        entry_point: &str,
        dispatch: [u32; 3],
        buffers: Vec<BufferConfig>,
    ) -> anyhow::Result<Vec<Vec<u8>>> {
        // Create shader module
        let module = self
            .device
            .create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("Compute Shader"),
                source: wgpu::ShaderSource::SpirV(Cow::Borrowed(spirv_words)),
            });

        // Create pipeline
        let pipeline = self
            .device
            .create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
                label: Some("Compute Pipeline"),
                layout: None,
                module: &module,
                entry_point: Some(entry_point),
                compilation_options: PipelineCompilationOptions::default(),
                cache: None,
            });

        // Create buffers
        let mut gpu_buffers = Vec::new();
        for (i, buffer_config) in buffers.iter().enumerate() {
            let usage = match buffer_config.usage {
                BufferUsage::Storage => wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC,
                BufferUsage::StorageReadOnly => {
                    wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC
                }
                BufferUsage::Uniform => wgpu::BufferUsages::UNIFORM,
            };

            let buffer = if let Some(initial_data) = &buffer_config.initial_data {
                self.device
                    .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some(&format!("Buffer {i}")),
                        contents: initial_data,
                        usage,
                    })
            } else {
                let buffer = self.device.create_buffer(&wgpu::BufferDescriptor {
                    label: Some(&format!("Buffer {i}")),
                    size: buffer_config.size,
                    usage,
                    mapped_at_creation: true,
                });
                {
                    // Zero the buffer
                    let initial_data = vec![0u8; buffer_config.size as usize];
                    let mut mapping = buffer.slice(..).get_mapped_range_mut();
                    mapping.copy_from_slice(&initial_data);
                }
                buffer.unmap();
                buffer
            };
            gpu_buffers.push(buffer);
        }

        // Create bind entries
        let bind_entries: Vec<_> = gpu_buffers
            .iter()
            .enumerate()
            .map(|(i, buffer)| wgpu::BindGroupEntry {
                binding: i as u32,
                resource: buffer.as_entire_binding(),
            })
            .collect();

        let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &pipeline.get_bind_group_layout(0),
            entries: &bind_entries,
            label: Some("Compute Bind Group"),
        });

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Compute Encoder"),
            });

        {
            let mut pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
                label: Some("Compute Pass"),
                timestamp_writes: Default::default(),
            });
            pass.set_pipeline(&pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.dispatch_workgroups(dispatch[0], dispatch[1], dispatch[2]);
        }

        // Create staging buffers and copy results
        let mut staging_buffers = Vec::new();
        for (i, buffer_config) in buffers.iter().enumerate() {
            if matches!(
                buffer_config.usage,
                BufferUsage::Storage | BufferUsage::StorageReadOnly
            ) {
                let staging_buffer = self.device.create_buffer(&wgpu::BufferDescriptor {
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

        self.queue.submit(Some(encoder.finish()));

        // Read back results
        let mut results = Vec::new();
        for staging_buffer in staging_buffers {
            if let Some(buffer) = staging_buffer {
                let buffer_slice = buffer.slice(..);
                let (sender, receiver) = futures::channel::oneshot::channel();
                buffer_slice.map_async(wgpu::MapMode::Read, move |res| {
                    let _ = sender.send(res);
                });
                self.device.poll(wgpu::PollType::wait_indefinitely())?;
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
}

impl<S> WgpuComputeTestMultiBuffer<S>
where
    S: WgpuShader,
{
    pub fn new(shader: S, dispatch: [u32; 3], buffers: Vec<BufferConfig>) -> Self {
        Self {
            shader,
            dispatch,
            buffers,
        }
    }

    pub fn new_with_sizes(shader: S, dispatch: [u32; 3], sizes: &[u64]) -> Self {
        let buffers = sizes
            .iter()
            .map(|&size| BufferConfig {
                size,
                usage: BufferUsage::Storage,
                initial_data: None,
            })
            .collect();
        Self::new(shader, dispatch, buffers)
    }

    pub fn run(self) -> anyhow::Result<Vec<Vec<u8>>> {
        let (device, queue) = WgpuComputeTest::<S>::init()?;
        let (module, entrypoint) = self.shader.create_module(&device)?;
        let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
            label: Some("Compute Pipeline"),
            layout: None,
            module: &module,
            entry_point: entrypoint.as_deref(),
            compilation_options: PipelineCompilationOptions::default(),
            cache: None,
        });

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

impl<S> WgpuComputeTestPushConstants<S>
where
    S: WgpuShader,
{
    pub fn new(
        shader: S,
        dispatch: [u32; 3],
        buffers: Vec<BufferConfig>,
        push_constants_size: u32,
        push_constants_data: Vec<u8>,
    ) -> Self {
        Self {
            shader,
            dispatch,
            buffers,
            push_constants_size,
            push_constants_data,
        }
    }

    pub fn run(self) -> anyhow::Result<Vec<Vec<u8>>> {
        let (device, queue) =
            WgpuComputeTest::<S>::init_with_features(wgpu::Features::PUSH_CONSTANTS)?;
        let (module, entrypoint) = self.shader.create_module(&device)?;

        // Create pipeline layout with push constants
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Bind Group Layout"),
            entries: &self
                .buffers
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
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[wgpu::PushConstantRange {
                stages: wgpu::ShaderStages::COMPUTE,
                range: 0..self.push_constants_size,
            }],
        });

        let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
            label: Some("Compute Pipeline"),
            layout: Some(&pipeline_layout),
            module: &module,
            entry_point: entrypoint.as_deref(),
            compilation_options: PipelineCompilationOptions::default(),
            cache: None,
        });

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
            layout: &bind_group_layout,
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
            pass.set_push_constants(0, &self.push_constants_data);
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
        let results = self.run()?;
        // Write first storage buffer output to file.
        for (data, buffer_config) in results.iter().zip(&buffers) {
            if buffer_config.usage == BufferUsage::Storage && !data.is_empty() {
                config.write_result(data)?;
                return Ok(());
            }
        }
        anyhow::bail!("No storage buffer output found")
    }
}

// Convenience implementation for WgpuComputeTestPushConstants
impl WgpuComputeTestPushConstants<RustComputeShader> {
    pub fn new_with_data<T: bytemuck::Pod>(
        shader: RustComputeShader,
        dispatch: [u32; 3],
        sizes: &[u64],
        push_constant_data: &T,
    ) -> Self {
        let buffers = sizes
            .iter()
            .map(|&size| BufferConfig {
                size,
                usage: BufferUsage::Storage,
                initial_data: None,
            })
            .collect();
        let push_constants_data = bytemuck::bytes_of(push_constant_data).to_vec();
        let push_constants_size = push_constants_data.len() as u32;

        Self::new(
            shader,
            dispatch,
            buffers,
            push_constants_size,
            push_constants_data,
        )
    }
}

impl WgpuComputeTestPushConstants<WgslComputeShader> {
    pub fn new_with_data<T: bytemuck::Pod>(
        shader: WgslComputeShader,
        dispatch: [u32; 3],
        sizes: &[u64],
        push_constant_data: &T,
    ) -> Self {
        let buffers = sizes
            .iter()
            .map(|&size| BufferConfig {
                size,
                usage: BufferUsage::Storage,
                initial_data: None,
            })
            .collect();
        let push_constants_data = bytemuck::bytes_of(push_constant_data).to_vec();
        let push_constants_size = push_constants_data.len() as u32;

        Self::new(
            shader,
            dispatch,
            buffers,
            push_constants_size,
            push_constants_data,
        )
    }
}
