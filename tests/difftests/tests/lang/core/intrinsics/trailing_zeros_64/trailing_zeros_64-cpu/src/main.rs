use difftest::config::{Config, TestMetadata};
use trailing_zeros_64_rust::TEST_DATA;

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    let output: Vec<u32> = TEST_DATA.iter().map(|v| v.trailing_zeros()).collect();

    config.write_result(&output).unwrap();
    config
        .write_metadata(&TestMetadata::u32())
        .expect("Failed to write metadata");
}
