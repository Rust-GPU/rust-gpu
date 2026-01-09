use difftest::config::{Config, TestMetadata};
use trailing_zeros_64_rust::TEST_DATA;

fn main() -> anyhow::Result<()> {
    let config = Config::new()?;

    let output: Vec<u32> = TEST_DATA.iter().map(|v| v.trailing_zeros()).collect();

    config.write_result(&output).unwrap();
    config.write_metadata(&TestMetadata::u32())?;
}
