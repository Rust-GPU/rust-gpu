use difftest::config::Config;
use difftest::scaffold::compute::{
    BufferConfig, BufferUsage, ComputeShaderTest, WgpuBackend, WgslComputeShader,
};

fn main() -> anyhow::Result<()> {
    let config = Config::new()?;

    // Create input data with specific patterns
    let mut input_data = vec![0u32; 256];
    for i in 0..256 {
        input_data[i] = i as u32;
    }
    // Set some specific values for indirect indexing test
    input_data[0] = 5;
    input_data[1] = 10;
    input_data[2] = 15;
    input_data[3] = 20;

    let input_bytes = bytemuck::cast_slice(&input_data).to_vec();

    let buffers = vec![
        BufferConfig {
            size: 1024, // 256 u32 values
            usage: BufferUsage::StorageReadOnly,
            initial_data: Some(input_bytes),
        },
        BufferConfig {
            size: 1024, // 256 u32 values for output
            usage: BufferUsage::Storage,
            initial_data: None,
        },
    ];

    ComputeShaderTest::<WgpuBackend, _>::new(
        WgslComputeShader::default(),
        [1, 1, 1], // Single workgroup with 64 threads
        buffers,
    )?
    .run_test(&config)
}
