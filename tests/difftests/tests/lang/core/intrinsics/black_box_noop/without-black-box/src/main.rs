use difftest::config::{Config, TestMetadata};
use difftest::scaffold::compute::{RustComputeShader, WgpuComputeTest};

fn main() -> anyhow::Result<()> {
    let config = Config::new()?;
    config.write_metadata(&TestMetadata::u32())?;
    let test = WgpuComputeTest::new(RustComputeShader::default(), [1, 1, 1], 12 * 4);
    test.run_test(&config)
}
