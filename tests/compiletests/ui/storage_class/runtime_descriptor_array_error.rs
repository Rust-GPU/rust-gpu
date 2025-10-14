// build-fail
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use spirv_std::{Image, RuntimeArray, spirv};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] one: &[Image!(2D, type=f32, sampled)],
    #[spirv(uniform, descriptor_set = 0, binding = 0)] two: &RuntimeArray<u32>,
) {
}
