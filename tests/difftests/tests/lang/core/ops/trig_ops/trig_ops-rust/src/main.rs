use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTest};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // 256 f32 inputs spread across [-1.2, 1.2]. The range is deliberately
    // kept away from ±π/2 so `tan` doesn't blow up, while still reaching
    // outside [-1, 1] so the asin/acos clamp branches actually clamp.
    let input_data: Vec<f32> = (0..256)
        .map(|i| {
            let t = (i as f32) / 255.0;
            (t * 2.0 - 1.0) * 1.2
        })
        .collect();

    let buffers = vec![
        BufferConfig::read_only(&input_data),
        BufferConfig::writeback(size_of_val(input_data.as_slice())),
    ];

    let test = WgpuComputeTest::new(RustComputeShader::default(), [4, 1, 1], buffers);

    config
        .write_metadata(&difftest::config::TestMetadata::f32(1e-4))
        .unwrap();

    test.run_test(&config).unwrap();
}
