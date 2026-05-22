use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, WgpuComputeTest, WgslComputeShader};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // 64 vec4<f32> inputs with all four components distinct, so every
    // swizzle permutation produces a different result (zeroed input would
    // make e.g. `.xyzw` and `.wzyx` indistinguishable).
    let input_data: Vec<[f32; 4]> = (0..64)
        .map(|i| {
            let base = i as f32 * 4.0;
            [base + 1.0, base + 2.0, base + 3.0, base + 4.0]
        })
        .collect();

    let buffers = vec![
        BufferConfig::read_only(&input_data),
        BufferConfig::writeback(size_of_val(input_data.as_slice())),
    ];

    let test = WgpuComputeTest::new(WgslComputeShader::default(), [1, 1, 1], buffers);

    test.run_test(&config).unwrap();
}
