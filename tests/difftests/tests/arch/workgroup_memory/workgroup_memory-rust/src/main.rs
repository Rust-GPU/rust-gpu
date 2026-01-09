#[cfg(not(target_arch = "spirv"))]
fn main() -> anyhow::Result<()> {
    use difftest::config::Config;
    use difftest::scaffold::compute::{
        BufferConfig, BufferUsage, ComputeShaderTest, RustComputeShader, WgpuBackend,
    };

    let config = Config::new()?;

    // Initialize input buffer with values to sum
    let input_data: Vec<u32> = (1..=64).collect();
    let input_bytes = bytemuck::cast_slice(&input_data).to_vec();

    let buffers = vec![
        BufferConfig {
            size: 256, // 64 u32 values
            usage: BufferUsage::StorageReadOnly,
            initial_data: Some(input_bytes),
        },
        BufferConfig {
            size: 4, // 1 u32 value for output
            usage: BufferUsage::Storage,
            initial_data: None,
        },
    ];

    // Write metadata for U32 comparison
    config.write_metadata(&difftest::config::TestMetadata::u32())?;

    ComputeShaderTest::<WgpuBackend, _>::new(
        RustComputeShader::default(),
        [1, 1, 1], // Single workgroup with 64 threads
        buffers,
    )?
    .run_test(&config)
}

#[cfg(target_arch = "spirv")]
fn main() -> anyhow::Result<()> {}
