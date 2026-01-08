use crate::common::{OUT_LEN, eval};
use difftest::config::Config;

pub fn run() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let mut out = vec![0; OUT_LEN];
    for gid in 0..OUT_LEN {
        eval(gid as u32, &mut out);
    }
    config.write_result(&out).unwrap()
}
