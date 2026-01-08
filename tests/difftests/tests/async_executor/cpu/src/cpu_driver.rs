use crate::common::{OUT_LEN, eval, input_dataset};
use difftest::config::Config;

pub fn run() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let input = input_dataset();
    let mut out = vec![0; OUT_LEN];
    eval(&input, &mut out).run_cpu(&input, &mut out);
    config.write_result(&out).unwrap()
}
