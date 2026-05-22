use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, WgpuComputeTest, WgslComputeShader};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // `input_a` is mixed with the Fibonacci-hashing constant (fractional bits
    // of 2^32 / golden ratio) so its bits spread across the full 32-bit range;
    // plain `i` would give `leading_zeros >= 24` on every thread and hide most
    // of the popcnt / ctlz / reverse_bits variation we're trying to diff.
    // `input_b` is just `i`, which for the first workgroup gives `b % 32`
    // values 0..31 so every shift / rotation amount is exercised.
    let input_a: Vec<u32> = (0..256)
        .map(|i| (i as u32).wrapping_mul(0x9E37_79B9))
        .collect();
    let input_b: Vec<u32> = (0..256).map(|i| i as u32).collect();

    let buffers = vec![
        BufferConfig::read_only(&input_a),
        BufferConfig::read_only(&input_b),
        BufferConfig::writeback(size_of_val(input_a.as_slice())),
    ];

    let test = WgpuComputeTest::new(WgslComputeShader::default(), [4, 1, 1], buffers);

    test.run_test(&config).unwrap();
}
