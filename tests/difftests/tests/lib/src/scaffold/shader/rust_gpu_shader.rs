use crate::scaffold::shader::{SpirvShader, WgpuShader};
use anyhow::Context;
use spirv_builder::{ModuleResult, SpirvBuilder};
use std::borrow::Cow;
use std::path::PathBuf;
use std::{env, fs};
use wgpu::ShaderSource;

/// A compute shader written in Rust compiled with spirv-builder.
pub struct RustComputeShader {
    pub builder: SpirvBuilder,
    pub passthrough: bool,
}

impl RustComputeShader {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self::with_target(path, "spirv-unknown-vulkan1.3")
    }

    pub fn with_target(path: impl Into<PathBuf>, target: impl Into<String>) -> Self {
        Self {
            builder: SpirvBuilder::new(path.into(), target)
                .release(true)
                .multimodule(false)
                .shader_panic_strategy(spirv_builder::ShaderPanicStrategy::SilentExit)
                .preserve_bindings(true),
            passthrough: false,
        }
    }

    pub fn with_capability(mut self, capability: spirv_builder::Capability) -> Self {
        self.builder.capabilities.push(capability);
        self
    }

    pub fn passthrough(self) -> Self {
        Self {
            passthrough: true,
            ..self
        }
    }
}

impl SpirvShader for RustComputeShader {
    fn spirv_bytes(&self) -> anyhow::Result<(Vec<u8>, String)> {
        let artifact = self
            .builder
            .build()
            .context("SpirvBuilder::build() failed")?;

        if artifact.entry_points.len() != 1 {
            anyhow::bail!(
                "Expected exactly one entry point, found {}",
                artifact.entry_points.len()
            );
        }
        let entry_point = artifact.entry_points.into_iter().next().unwrap();

        let shader_bytes = match artifact.module {
            ModuleResult::SingleModule(path) => fs::read(&path)
                .with_context(|| format!("reading spv file '{}' failed", path.display()))?,
            ModuleResult::MultiModule(_modules) => {
                anyhow::bail!("MultiModule modules produced");
            }
        };

        Ok((shader_bytes, entry_point))
    }
}

impl WgpuShader for RustComputeShader {
    fn create_pipeline(
        &self,
        device: &wgpu::Device,
        layout: &wgpu::PipelineLayout,
    ) -> anyhow::Result<wgpu::ComputePipeline> {
        let (shader_bytes, entry_point) = self.spirv_bytes()?;

        if !shader_bytes.len().is_multiple_of(4) {
            anyhow::bail!("SPIR-V binary length is not a multiple of 4");
        }
        let shader_words: Vec<u32> = bytemuck::cast_slice(&shader_bytes).to_vec();
        let module = if self.passthrough {
            unsafe {
                device.create_shader_module_passthrough(wgpu::ShaderModuleDescriptorPassthrough {
                    label: Some("Rust-GPU Compute Shader"),
                    num_workgroups: (0, 0, 0),
                    spirv: Some(Cow::Owned(shader_words)),
                    dxil: None,
                    msl: None,
                    hlsl: None,
                    glsl: None,
                    wgsl: None,
                    metallib: None,
                })
            }
        } else {
            device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("Rust-GPU Compute Shader"),
                source: ShaderSource::SpirV(Cow::Owned(shader_words)),
            })
        };
        let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
            label: Some("Compute Pipeline"),
            layout: Some(layout),
            module: &module,
            entry_point: Some(&entry_point),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            cache: None,
        });
        Ok(pipeline)
    }
}

/// For the SPIR-V shader, the manifest directory is used as the build path.
impl Default for RustComputeShader {
    fn default() -> Self {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
        Self::new(PathBuf::from(manifest_dir))
    }
}
