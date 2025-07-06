use difftest::config::Config;
use difftest::scaffold::compute::{RustComputeShader, WgpuComputeTest};

fn main() {
    // Load the config from the harness.
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // Define test parameters, loading the rust shader from the current crate.
    // Dispatch 2x2x1 workgroups with workgroup size [8, 4, 2]
    // This gives us a total of 2*2*1 * 8*4*2 = 256 invocations
    let test = WgpuComputeTest::new(RustComputeShader::default(), [2, 2, 1], 1024);

    // Run the test and write the output to a file.
    test.run_test(&config).unwrap();
}
