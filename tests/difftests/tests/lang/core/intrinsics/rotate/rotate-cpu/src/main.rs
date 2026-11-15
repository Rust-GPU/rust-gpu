use difftest::config::{Config, TestMetadata};
use rotate_rust::{rotate_case, test_data};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    let output: Vec<u32> = test_data()
        .iter()
        .flat_map(|&[value, amount]| rotate_case(value, amount as u32))
        .collect();

    config.write_result(&output).unwrap();
    config
        .write_metadata(&TestMetadata::u32())
        .expect("Failed to write metadata");
}
