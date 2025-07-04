#[cfg(not(target_arch = "spirv"))]
fn main() {
    use difftest::config::Config;
    use difftest::scaffold::compute::{
        BufferConfig, BufferUsage, RustComputeShader, WgpuComputeTestMultiBuffer,
    };

    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

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

    let test = WgpuComputeTestMultiBuffer::new(
        RustComputeShader::default(),
        [1, 1, 1], // Single workgroup with 32 threads
        buffers,
    );

    test.run_test(&config).unwrap();
}

#[cfg(target_arch = "spirv")]
fn main() {}
