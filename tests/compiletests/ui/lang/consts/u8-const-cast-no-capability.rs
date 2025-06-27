// build-pass
// Test that u8 constants cast to u32 don't require Int8 capability when optimized away

#![no_std]
use spirv_std::spirv;

const K: u8 = 20;

#[spirv(fragment)]
pub fn main(output: &mut u32) {
    // This should not require Int8 capability as K is only used as u32
    // and the optimization should fold the constant cast
    *output = K as u32;
}
