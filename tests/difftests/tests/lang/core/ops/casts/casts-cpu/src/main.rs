use casts_gpu::{TEST_DATA, test};
use difftest::config::{Config, TestMetadata};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    let output: Vec<u32> = TEST_DATA.iter().flat_map(|&value| test(value)).collect();

    config.write_result(&output).unwrap();
    config
        .write_metadata(&TestMetadata::u32())
        .expect("Failed to write metadata");
}
