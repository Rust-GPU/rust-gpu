use difftest::config::{Config, TestMetadata};
use fill_offset_zero_rust::{INPUT, eval};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    config.write_result(eval(&INPUT).as_flattened()).unwrap();
    config.write_metadata(&TestMetadata::u32()).unwrap();
}
