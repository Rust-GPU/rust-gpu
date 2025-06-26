// compile-flags: -C llvm-args=--allow-fragment-no-output
// build-fail

use spirv_std::spirv;

#[spirv(fragment)]
pub fn fragment(int: u32, double: f64) {}
