use difftest::config::Config;
use difftest::scaffold::compute::{WgpuComputeTestMultiBuffer, WgslComputeShader};

fn main() {
    // Load the config from the harness.
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // Define test parameters, loading the wgsl shader from the crate directory.
    let test = WgpuComputeTestMultiBuffer::new_single_buffer(
        WgslComputeShader::default(),
        [1, 1, 1],
        1024,
    );

    // Run the test and write the output to a file.
    test.run_test(&config).unwrap();
}
