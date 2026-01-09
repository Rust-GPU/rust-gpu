#[cfg(not(target_arch = "spirv"))]
fn main() -> anyhow::Result<()> {
    use difftest::config::Config;
    use difftest::scaffold::compute::{
        BufferConfig, BufferUsage, ComputeShaderTest, RustComputeShader, WgpuBackend,
    };

    let config = Config::new()?;

    // Create input data with various float values
    let input_data: Vec<f32> = (0..32)
        .map(|i| match i {
            0 => 0.0,
            1 => 1.0,
            2 => -1.0,
            3 => 0.5,
            4 => -0.5,
            5 => 2.0,
            6 => -2.0,
            7 => std::f32::consts::PI,
            8 => std::f32::consts::E,
            9 => 3.0,
            10 => -3.0,
            11 => 0.1,
            12 => -0.1,
            13 => 4.0,
            14 => -4.0,
            15 => 3.14159,
            _ => (i as f32) * 0.1 - 1.5,
        })
        .collect();

    let input_bytes = bytemuck::cast_slice(&input_data).to_vec();

    let buffers = vec![
        BufferConfig {
            size: 128, // 32 f32 values
            usage: BufferUsage::StorageReadOnly,
            initial_data: Some(input_bytes),
        },
        BufferConfig {
            size: 2688, // 672 f32 values (32 threads * 21 outputs each)
            usage: BufferUsage::Storage,
            initial_data: None,
        },
    ];

    // Write metadata file
    config.write_metadata(&difftest::config::TestMetadata::f32(2e-6))?;

    ComputeShaderTest::<WgpuBackend, _>::new(
        RustComputeShader::default(),
        [1, 1, 1], // Single workgroup with 32 threads
        buffers,
    )?
    .run_test(&config)
}

#[cfg(target_arch = "spirv")]
fn main() -> anyhow::Result<()> {}
