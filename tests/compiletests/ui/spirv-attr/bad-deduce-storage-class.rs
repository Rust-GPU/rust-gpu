// Tests that storage class deduction (from entry-point signature) fails correctly
// build-fail

use spirv_std::{Image, spirv};

#[spirv(vertex)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, uniform)] error: &Image!(2D, type=f32),
    #[spirv(descriptor_set = 0, binding = 1, uniform_constant)] warning: &Image!(2D, type=f32),
) {
}

// https://github.com/EmbarkStudios/rust-gpu/issues/585
#[spirv(vertex)]
pub fn issue_585(#[spirv(descriptor_set = 0, binding = 0)] invalid: Image!(2D, type=f32)) {}
