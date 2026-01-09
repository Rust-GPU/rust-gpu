use difftest::config::Config;
use difftest::scaffold::compute::{
    BufferConfig, BufferUsage, ComputeShaderTest, WgpuBackend, WgslComputeShader,
};

fn main() -> anyhow::Result<()> {
    let config = Config::new()?;

    // Create input data with various float values
    let input_data: Vec<f32> = (0..128)
        .map(|i| match i % 16 {
            0 => 0.0,
            1 => 1.0,
            2 => -1.0,
            3 => 0.5,
            4 => -0.5,
            5 => 2.0,
            6 => -2.0,
            7 => 3.0,
            8 => std::f32::consts::PI,
            9 => std::f32::consts::E,
            10 => 0.1,
            11 => -0.1,
            12 => 4.0,
            13 => -4.0,
            14 => 0.25,
            15 => -0.25,
            _ => unreachable!(),
        })
        .collect();

    let input_bytes = bytemuck::cast_slice(&input_data).to_vec();

    let buffers = vec![
        BufferConfig {
            size: 512, // 128 f32 values
            usage: BufferUsage::StorageReadOnly,
            initial_data: Some(input_bytes),
        },
        BufferConfig {
            size: 6400, // 1600 f32 values (32 threads * 50 outputs each)
            usage: BufferUsage::Storage,
            initial_data: None,
        },
    ];

    // Write metadata file
    config.write_metadata(&difftest::config::TestMetadata::f32(1e-5))?;

    ComputeShaderTest::<WgpuBackend, _>::new(
        WgslComputeShader::default(),
        [1, 1, 1], // Single workgroup with 32 threads
        buffers,
    )?
    .run_test(&config)
}
