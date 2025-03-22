// Test log10 intrinsic
// build-pass

#![allow(internal_features)]
#![feature(core_intrinsics)]
#![no_std]

use spirv_std::num_traits::Float as _;
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[f32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [f32],
) {
    output[0] = input[0].log10();
}
