mod rust_gpu_shader;
mod wgsl_shader;

pub use rust_gpu_shader::RustComputeShader;
pub use wgsl_shader::WgslComputeShader;

/// Trait for shaders that can provide SPIRV bytes.
pub trait SpirvShader {
    /// Returns the SPIRV bytes and entry point name.
    fn spirv_bytes(&self) -> anyhow::Result<(Vec<u32>, String)>;
}

/// Trait for shaders that can create wgpu modules.
pub trait WgpuShader {
    /// Creates a wgpu shader module.
    fn create_module(
        &self,
        device: &wgpu::Device,
    ) -> anyhow::Result<(wgpu::ShaderModule, Option<String>)>;
}
