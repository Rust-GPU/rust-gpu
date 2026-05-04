// build-pass
// compile-flags: -C target-feature=+Float16
#![feature(f16)]

use spirv_std::num_traits::Float;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: u32, o: &mut f32) {
    let a = [0.123f16; 16];
    *o = a[i as usize] as f32;
}
