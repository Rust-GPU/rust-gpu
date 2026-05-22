use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTest};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // 64 vec4<f32> inputs with distinct component values so each dynamic
    // extract/insert case produces an observable difference.
    let input_data: Vec<[f32; 4]> = (0..64)
        .map(|i| {
            let base = i as f32 * 4.0;
            [base + 1.0, base + 2.0, base + 3.0, base + 4.0]
        })
        .collect();

    // The shader picks a switch case with `tid % 8` and a dynamic index with
    // `indices[tid] % 4`. The 8 threads inside each case have tid stride 8,
    // so `indices[i] = i / 8` makes them see all four `index % 4` values
    // (0, 1, 2, 3, 0, 1, 2, 3) — any linear function of `i` would collapse
    // to a single index per case.
    let indices_data: Vec<u32> = (0..256).map(|i| (i as u32) / 8).collect();

    let buffers = vec![
        BufferConfig::read_only(&input_data),
        BufferConfig::read_only(&indices_data),
        BufferConfig::writeback(size_of_val(input_data.as_slice())),
    ];

    let test = WgpuComputeTest::new(RustComputeShader::default(), [1, 1, 1], buffers);

    test.run_test(&config).unwrap();
}
