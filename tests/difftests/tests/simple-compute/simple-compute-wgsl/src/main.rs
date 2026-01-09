use difftest::config::Config;
use difftest::scaffold::compute::{WgpuComputeTest, WgslComputeShader};

fn main() -> anyhow::Result<()> {
    // Load the config from the harness.
    let config = Config::new()?;

    // Define test parameters, loading the wgsl shader from the crate directory.
    let test = WgpuComputeTest::new(WgslComputeShader::default(), [1, 1, 1], 1024);

    // Run the test and write the output to a file.
    test.run_test(&config)
}
