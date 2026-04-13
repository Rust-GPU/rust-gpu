use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTestMultiBuffer};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // 256 u32 inputs covering every branch of `process_value`.
    let input_data: Vec<u32> = (0..256)
        .map(|i| match i {
            // Hits the `value == 0` early return.
            0 => 0,
            // Hit the `value > 1000` early return with differing subtraction magnitudes.
            1 => 1001,
            2 => 2500,
            // Values 3..=255 cycle through all ten `value % 10` arms, straddle the
            // `< 50` / `>= 50` split for the final transform, and keep case 9's
            // `for i in 0..value` loop bounded.
            _ => i as u32,
        })
        .collect();

    let buffers = vec![
        BufferConfig::read_only(&input_data),
        BufferConfig::writeback(size_of_val(input_data.as_slice())),
    ];

    let test = WgpuComputeTestMultiBuffer::new(RustComputeShader::default(), [4, 1, 1], buffers);

    test.run_test(&config).unwrap();
}
