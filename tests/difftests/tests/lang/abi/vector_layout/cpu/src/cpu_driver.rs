use crate::layout::eval_layouts;
use difftest::config::Config;

pub fn run() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    config
        .write_result(bytemuck::bytes_of(&eval_layouts()))
        .unwrap()
}
