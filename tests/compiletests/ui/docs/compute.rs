// built-pass

use spirv_std::spirv;

#[spirv(compute(threads(8, 8)))]
pub fn main() {}
