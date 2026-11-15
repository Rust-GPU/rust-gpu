use bit_ops_rust::{TEST_DATA, leading_zeros_case};
use difftest::config::{Config, TestMetadata};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    let output: Vec<u32> = TEST_DATA
        .iter()
        .flat_map(|&value| leading_zeros_case(value))
        .collect();

    config.write_result(&output).unwrap();
    config
        .write_metadata(&TestMetadata::u32())
        .expect("Failed to write metadata");
}
