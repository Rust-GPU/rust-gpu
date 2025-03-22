use crate::config::Config;
use bytemuck::Pod;
use futures::{channel::oneshot::Canceled, executor::block_on};
use spirv_builder::{ModuleResult, SpirvBuilder};
use std::{
    borrow::Cow,
    env,
    fs::{self, File},
    io::Write,
    path::PathBuf,
};
use thiserror::Error;
use wgpu::{BufferAsyncError, PipelineCompilationOptions, util::DeviceExt};

#[derive(Error, Debug)]
pub enum ComputeError {
    #[error("Failed to find a suitable GPU adapter")]
    AdapterNotFound,
    #[error("Failed to create device: {0}")]
    DeviceCreationFailed(String),
    #[error("Failed to load shader: {0}")]
    ShaderLoadFailed(String),
    #[error("Mapping compute output future canceled: {0}")]
    MappingCanceled(Canceled),
    #[error("Mapping compute output failed: {0}")]
    MappingFailed(BufferAsyncError),
}

/// Trait that creates a shader module and provides its entry point.
pub trait ComputeShader {
    fn create_module(
        &self,
        device: &wgpu::Device,
    ) -> Result<(wgpu::ShaderModule, Option<String>), ComputeError>;
}

/// A compute shader written in Rust compiled with spirv-builder.
pub struct RustComputeShader {
    pub path: PathBuf,
}

impl RustComputeShader {
    pub fn new<P: Into<PathBuf>>(path: P) -> Self {
        Self { path: path.into() }
    }
}

impl ComputeShader for RustComputeShader {
    fn create_module(
        &self,
        device: &wgpu::Device,
    ) -> Result<(wgpu::ShaderModule, Option<String>), ComputeError> {
        let builder = SpirvBuilder::new(&self.path, "spirv-unknown-vulkan1.1")
            .print_metadata(spirv_builder::MetadataPrintout::None)
            .release(true)
            .multimodule(false)
            .shader_panic_strategy(spirv_builder::ShaderPanicStrategy::SilentExit)
            .preserve_bindings(true);
        let artifact = builder
            .build()
            .map_err(|e| ComputeError::ShaderLoadFailed(e.to_string()))?;

        if artifact.entry_points.len() != 1 {
            return Err(ComputeError::ShaderLoadFailed(format!(
                "Expected exactly one entry point, found {}",
                artifact.entry_points.len()
            )));
        }
        let entry_point = artifact.entry_points.into_iter().next().unwrap();

        let shader_bytes = match artifact.module {
            ModuleResult::SingleModule(path) => {
                fs::read(&path).map_err(|e| ComputeError::ShaderLoadFailed(e.to_string()))?
            }
            ModuleResult::MultiModule(_modules) => {
                return Err(ComputeError::ShaderLoadFailed(
                    "Multiple modules produced".to_string(),
                ));
            }
        };

        if shader_bytes.len() % 4 != 0 {
            return Err(ComputeError::ShaderLoadFailed(
                "SPIR-V binary length is not a multiple of 4".to_string(),
            ));
        }
        let shader_words: Vec<u32> = bytemuck::cast_slice(&shader_bytes).to_vec();
        let module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Compute Shader"),
            source: wgpu::ShaderSource::SpirV(Cow::Owned(shader_words)),
        });
        Ok((module, Some(entry_point)))
    }
}

/// A WGSL compute shader.
pub struct WgslComputeShader {
    pub path: PathBuf,
    pub entry_point: Option<String>,
}

impl WgslComputeShader {
    pub fn new<P: Into<PathBuf>>(path: P, entry_point: Option<String>) -> Self {
        Self {
            path: path.into(),
            entry_point,
        }
    }
}

impl ComputeShader for WgslComputeShader {
    fn create_module(
        &self,
        device: &wgpu::Device,
    ) -> Result<(wgpu::ShaderModule, Option<String>), ComputeError> {
        let shader_source = fs::read_to_string(&self.path)
            .map_err(|e| ComputeError::ShaderLoadFailed(e.to_string()))?;
        let module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Compute Shader"),
            source: wgpu::ShaderSource::Wgsl(Cow::Owned(shader_source)),
        });
        Ok((module, self.entry_point.clone()))
    }
}

/// Compute test that is generic over the shader type.
pub struct WgpuComputeTest<S> {
    shader: S,
    dispatch: [u32; 3],
    output_bytes: u64,
}

impl<S> WgpuComputeTest<S>
where
    S: ComputeShader,
{
    pub fn new(shader: S, dispatch: [u32; 3], output_bytes: u64) -> Self {
        Self {
            shader,
            dispatch,
            output_bytes,
        }
    }

    fn init() -> Result<(wgpu::Device, wgpu::Queue), ComputeError> {
        block_on(async {
            let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
                #[cfg(target_os = "linux")]
                backends: wgpu::Backends::VULKAN,
                #[cfg(not(target_os = "linux"))]
                backends: wgpu::Backends::PRIMARY,
                dx12_shader_compiler: Default::default(),
                flags: Default::default(),
                gles_minor_version: Default::default(),
            });
            let adapter = instance
                .request_adapter(&wgpu::RequestAdapterOptions {
                    power_preference: wgpu::PowerPreference::HighPerformance,
                    compatible_surface: None,
                    force_fallback_adapter: false,
                })
                .await
                .ok_or(ComputeError::AdapterNotFound)?;
            let (device, queue) = adapter
                .request_device(
                    &wgpu::DeviceDescriptor {
                        label: Some("wgpu Device"),
                        #[cfg(target_os = "linux")]
                        required_features: wgpu::Features::SPIRV_SHADER_PASSTHROUGH,
                        #[cfg(not(target_os = "linux"))]
                        required_features: wgpu::Features::empty(),
                        required_limits: wgpu::Limits::default(),
                        memory_hints: Default::default(),
                    },
                    None,
                )
                .await
                .map_err(|e| ComputeError::DeviceCreationFailed(e.to_string()))?;
            Ok((device, queue))
        })
    }

    fn run_internal<I>(self, input: Option<I>) -> Result<Vec<u8>, ComputeError>
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
        device.poll(wgpu::Maintain::Wait);
        block_on(receiver)
            .map_err(ComputeError::MappingCanceled)?
            .map_err(ComputeError::MappingFailed)?;
        let data = buffer_slice.get_mapped_range().to_vec();
        staging_buffer.unmap();
        Ok(data)
    }

    /// Runs the compute shader with no input.
    pub fn run(self) -> Result<Vec<u8>, ComputeError> {
        self.run_internal::<()>(None)
    }

    /// Runs the compute shader with provided input.
    pub fn run_with_input<I>(self, input: I) -> Result<Vec<u8>, ComputeError>
    where
        I: Sized + Pod,
    {
        self.run_internal(Some(input))
    }

    /// Runs the compute shader with no input and writes the output to a file.
    pub fn run_test(self, config: &Config) -> Result<(), ComputeError> {
        let output = self.run()?;
        let mut f = File::create(&config.output_path).unwrap();
        f.write_all(&output).unwrap();
        Ok(())
    }

    /// Runs the compute shader with provided input and writes the output to a file.
    pub fn run_test_with_input<I>(self, config: &Config, input: I) -> Result<(), ComputeError>
    where
        I: Sized + Pod,
    {
        let output = self.run_with_input(input)?;
        let mut f = File::create(&config.output_path).unwrap();
        f.write_all(&output).unwrap();
        Ok(())
    }
}

/// For WGSL, the code checks for "shader.wgsl" then "compute.wgsl".
impl Default for WgslComputeShader {
    fn default() -> Self {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
        let manifest_path = PathBuf::from(manifest_dir);
        let shader_path = manifest_path.join("shader.wgsl");
        let compute_path = manifest_path.join("compute.wgsl");

        let (file, source) = if shader_path.exists() {
            (
                shader_path.clone(),
                fs::read_to_string(&shader_path).unwrap_or_default(),
            )
        } else if compute_path.exists() {
            (
                compute_path.clone(),
                fs::read_to_string(&compute_path).unwrap_or_default(),
            )
        } else {
            panic!("No default WGSL shader found in manifest directory");
        };

        let entry_point = if source.contains("fn main_cs(") {
            Some("main_cs".to_string())
        } else if source.contains("fn main(") {
            Some("main".to_string())
        } else {
            None
        };

        Self::new(file, entry_point)
    }
}

/// For the SPIR-V shader, the manifest directory is used as the build path.
impl Default for RustComputeShader {
    fn default() -> Self {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
        Self::new(PathBuf::from(manifest_dir))
    }
}
