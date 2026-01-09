use difftest::config::Config;
use difftest::scaffold::compute::{RustComputeShader, WgpuComputeTest};

fn main() -> anyhow::Result<()> {
    // Load the config from the harness.
    let config = Config::new()?;

    // Define test parameters, loading the rust shader from the current crate.
    let test = WgpuComputeTest::new(RustComputeShader::default(), [1, 1, 1], 1024);

    // Run the test and write the output to a file.
    test.run_test(&config)
}
