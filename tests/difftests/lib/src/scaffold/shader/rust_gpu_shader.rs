use crate::scaffold::shader::{SpirvShader, WgpuShader};
use anyhow::Context;
use spirv_builder::{ModuleResult, SpirvBuilder};
use std::borrow::Cow;
use std::path::PathBuf;
use std::{env, fs};

/// A compute shader written in Rust compiled with spirv-builder.
pub struct RustComputeShader {
    pub path: PathBuf,
    pub target: String,
    pub capabilities: Vec<spirv_builder::Capability>,
}

impl RustComputeShader {
    pub fn new<P: Into<PathBuf>>(path: P) -> Self {
        Self {
            path: path.into(),
            target: "spirv-unknown-vulkan1.1".to_string(),
            capabilities: Vec::new(),
        }
    }

    pub fn with_target<P: Into<PathBuf>>(path: P, target: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            target: target.into(),
            capabilities: Vec::new(),
        }
    }

    pub fn with_capability(mut self, capability: spirv_builder::Capability) -> Self {
        self.capabilities.push(capability);
        self
    }
}

impl SpirvShader for RustComputeShader {
    fn spirv_bytes(&self) -> anyhow::Result<(Vec<u8>, String)> {
        let mut builder = SpirvBuilder::new(&self.path, &self.target)
            .print_metadata(spirv_builder::MetadataPrintout::None)
            .release(true)
            .multimodule(false)
            .shader_panic_strategy(spirv_builder::ShaderPanicStrategy::SilentExit)
            .preserve_bindings(true);

        for capability in &self.capabilities {
            builder = builder.capability(*capability);
        }

        let artifact = builder.build().context("SpirvBuilder::build() failed")?;

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
    fn create_module(
        &self,
        device: &wgpu::Device,
    ) -> anyhow::Result<(wgpu::ShaderModule, Option<String>)> {
        let (shader_bytes, entry_point) = self.spirv_bytes()?;

        if !shader_bytes.len().is_multiple_of(4) {
            anyhow::bail!("SPIR-V binary length is not a multiple of 4");
        }
        let shader_words: Vec<u32> = bytemuck::cast_slice(&shader_bytes).to_vec();
        let module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Compute Shader"),
            source: wgpu::ShaderSource::SpirV(Cow::Owned(shader_words)),
        });
        Ok((module, Some(entry_point)))
    }
}

/// For the SPIR-V shader, the manifest directory is used as the build path.
impl Default for RustComputeShader {
    fn default() -> Self {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
        Self::new(PathBuf::from(manifest_dir))
    }
}
