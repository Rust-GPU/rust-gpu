use difftest::config::{Config, TestMetadata};
use unbounded_shift_rust::{test_data, unbounded_shift_case};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    let output: Vec<u32> = test_data()
        .iter()
        .flat_map(|&[value, amount]| unbounded_shift_case(value, amount as u32))
        .collect();

    config.write_result(&output).unwrap();
    config
        .write_metadata(&TestMetadata::u32())
        .expect("Failed to write metadata");
}
