mod rust_gpu_shader;
mod wgsl_shader;

pub use rust_gpu_shader::RustComputeShader;
pub use wgsl_shader::WgslComputeShader;

/// Trait for shaders that can provide SPIRV bytes.
pub trait SpirvShader {
    /// Returns the SPIRV bytes and entry point name.
    fn spirv_bytes(&self) -> anyhow::Result<(Vec<u8>, String)>;
}

/// Trait for shaders that can create wgpu modules.
pub trait WgpuShader {
    /// Creates a wgpu shader module.
    fn create_pipeline(
        &self,
        device: &wgpu::Device,
        pipeline_layout: &wgpu::PipelineLayout,
    ) -> anyhow::Result<wgpu::ComputePipeline>;

    // HACK(eddyb) used for hooking `spirti` into `wgpu_runner`.
    fn maybe_spirv_bytes(&self) -> anyhow::Result<Option<(Vec<u8>, String)>> {
        Ok(None)
    }
}
