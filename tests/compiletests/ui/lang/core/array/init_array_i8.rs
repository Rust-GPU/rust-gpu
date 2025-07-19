// Test creating an array.
// build-pass
// compile-flags: -C target-feature=+Int8

use spirv_std::macros::spirv;

#[spirv(fragment)]
pub fn main(o: &mut i8) {
    let array = [0i8; 4];
    *o = array[1];
}
