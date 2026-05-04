use checked_mul_shader::{INPUTS, OUTPUT_LEN, PAIR_COUNT};
use difftest::config::Config;

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    let mut output = vec![0u32; OUTPUT_LEN];
    for i in 0..PAIR_COUNT {
        let a = INPUTS[2 * i];
        let b = INPUTS[2 * i + 1];
        let (ur, uo) = a.overflowing_mul(b);
        output[4 * i] = ur;
        output[4 * i + 1] = uo as u32;
        let (sr, so) = (a as i32).overflowing_mul(b as i32);
        output[4 * i + 2] = sr as u32;
        output[4 * i + 3] = so as u32;
    }

    config.write_result(&output).unwrap();
}
