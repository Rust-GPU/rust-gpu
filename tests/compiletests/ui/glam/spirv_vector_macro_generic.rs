// build-pass
// only-vulkan1.2
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformShuffleRelative,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""

use spirv_std::Scalar;
use spirv_std::arch::subgroup_shuffle_up;
use spirv_std::glam::Vec3;
use spirv_std::spirv;

#[spirv_std::spirv_vector]
#[derive(Copy, Clone, Default)]
pub struct Vec<T: Scalar> {
    pub x: T,
    pub y: T,
    pub z: T,
}

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &Vec<i32>,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut Vec<f32>,
) {
    let vec = Vec {
        x: input.x as f32,
        y: input.y as f32,
        z: input.z as f32,
    };
    // any function that accepts a `VectorOrScalar` would do
    *output = subgroup_shuffle_up(vec, 5);
}
