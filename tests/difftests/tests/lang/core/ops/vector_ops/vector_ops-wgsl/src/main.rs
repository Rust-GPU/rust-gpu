use difftest::config::Config;
use difftest::scaffold::compute::{
    BufferConfig, BufferUsage, WgpuComputeTestMultiBuffer, WgslComputeShader,
};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

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

    let test = WgpuComputeTestMultiBuffer::new(
        WgslComputeShader::default(),
        [1, 1, 1], // Single workgroup with 32 threads
        buffers,
    );

    // Write metadata file
    let metadata = difftest::config::TestMetadata {
        epsilon: Some(1e-5), // 1e-5 - appropriate for 5 decimal place rounding
        output_type: difftest::config::OutputType::F32,
    };
    config.write_metadata(&metadata).unwrap();

    test.run_test(&config).unwrap();
}
