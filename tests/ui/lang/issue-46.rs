// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    let x = [[1; 2]; 1];
}
