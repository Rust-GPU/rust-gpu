use difftest::config::Config;
use difftest::scaffold::compute::{WgpuComputeTestMultiBuffer, WgslComputeShader};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    let buffer_size = 1024;
    let test = WgpuComputeTestMultiBuffer::new_with_sizes(
        WgslComputeShader::default(),
        [64, 1, 1],
        &[buffer_size, buffer_size],
    );

    test.run_test(&config).unwrap();
}
