// build-pass
// compile-flags: -C llvm-args=--allow-fragment-no-output

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: i32) {
    if i > 0 {
    } else if i < 0 {
    } else {
    }
}
