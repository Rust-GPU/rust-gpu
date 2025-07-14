#[cfg(not(target_arch = "spirv"))]
fn main() {
    use difftest::config::Config;
    use difftest::scaffold::compute::{
        BufferConfig, BufferUsage, RustComputeShader, WgpuComputeTestMultiBuffer,
    };

    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // Create input data with various float values
    let input_data: Vec<f32> = (0..128)
        .map(|i| match i % 16 {
            0 => 1.0,
            1 => 2.0,
            2 => 3.0,
            3 => 4.0,
            4 => 0.5,
            5 => -0.5,
            6 => 2.0,
            7 => -2.0,
            8 => 0.0,
            9 => 1.0,
            10 => -1.0,
            11 => 0.1,
            12 => 3.14,
            13 => 2.71,
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

    let test = WgpuComputeTestMultiBuffer::new(
        RustComputeShader::default(),
        [1, 1, 1], // Single workgroup with 32 threads
        buffers,
    );

    // Write metadata file
    // this one requires higher epsilon, noticed on an RDNA2 680M (Ryzen iGPU) with radv
    config
        .write_metadata(&difftest::config::TestMetadata::f32(2e-5))
        .unwrap();

    test.run_test(&config).unwrap();
}

#[cfg(target_arch = "spirv")]
fn main() {}
