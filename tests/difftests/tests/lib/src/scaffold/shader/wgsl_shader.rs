use crate::scaffold::shader::WgpuShader;
use anyhow::Context;
use std::borrow::Cow;
use std::path::PathBuf;
use std::{env, fs};

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

impl WgpuShader for WgslComputeShader {
    fn create_module(
        &self,
        device: &wgpu::Device,
    ) -> anyhow::Result<(wgpu::ShaderModule, Option<String>)> {
        let shader_source = fs::read_to_string(&self.path)
            .with_context(|| format!("reading wgsl source file '{}'", &self.path.display()))?;
        let module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Compute Shader"),
            source: wgpu::ShaderSource::Wgsl(Cow::Owned(shader_source)),
        });
        Ok((module, self.entry_point.clone()))
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
