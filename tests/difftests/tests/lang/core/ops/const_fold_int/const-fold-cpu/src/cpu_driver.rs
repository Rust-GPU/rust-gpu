use crate::{INTERESTING_PATTERNS, Variants};
use difftest::config::Config;

pub fn run(variant: Variants) {
    let config = Config::new()?;
    let result = variant
        .eval(&INTERESTING_PATTERNS)
        .into_iter()
        .flatten()
        .flatten()
        .collect::<Vec<_>>();
    config.write_result(&result).unwrap()
}
