use crate::{INTERESTING_PATTERNS, Variants};
use difftest::config::Config;

pub fn run(variant: Variants) {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let result = variant
        .eval(&INTERESTING_PATTERNS)
        .into_iter()
        .flatten()
        .flatten()
        .collect::<Vec<_>>();
    config.write_result(&result).unwrap()
}
