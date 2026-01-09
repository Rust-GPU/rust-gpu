#[cfg(not(target_arch = "spirv"))]
fn main() -> anyhow::Result<()> {
    use difftest::config::Config;
    use difftest::scaffold::compute::{
        BufferConfig, BufferUsage, ComputeShaderTest, RustComputeShader, WgpuBackend,
    };

    let config = Config::new()?;

    // Create input data with various values to test different control flow paths
    let input_data: Vec<u32> = (0..64).map(|i| i as u32).collect();
    let input_bytes = bytemuck::cast_slice(&input_data).to_vec();

    let buffers = vec![
        BufferConfig {
            size: 256, // 64 u32 values
            usage: BufferUsage::StorageReadOnly,
            initial_data: Some(input_bytes),
        },
        BufferConfig {
            size: 256, // 64 u32 values for output
            usage: BufferUsage::Storage,
            initial_data: None,
        },
    ];

    ComputeShaderTest::<WgpuBackend, _>::new(
        RustComputeShader::default(),
        [1, 1, 1], // Single workgroup with 64 threads
        buffers,
    )?
    .run_test(&config)
}

#[cfg(target_arch = "spirv")]
fn main() -> anyhow::Result<()> {}
