use difftest::config::Config;
use difftest::scaffold::compute::{
    BufferConfig, BufferUsage, ComputeShaderTest, RustComputeShader, WgpuBackend,
};

fn main() -> anyhow::Result<()> {
    let config = Config::new()?;

    // Initialize counter buffer with test values
    let counter_data = vec![100u32, 50, 20, 5, 0];
    let counter_bytes = bytemuck::cast_slice(&counter_data).to_vec();

    let buffers = vec![
        BufferConfig {
            size: 20, // 5 u32 values
            usage: BufferUsage::Storage,
            initial_data: Some(counter_bytes),
        },
        BufferConfig {
            size: 20, // 5 u32 values for output
            usage: BufferUsage::Storage,
            initial_data: None,
        },
    ];

    ComputeShaderTest::<WgpuBackend, _>::new(
        RustComputeShader::default(),
        [1, 1, 1], // Single workgroup with 32 threads
        buffers,
    )?
    .run_test(&config)
}
