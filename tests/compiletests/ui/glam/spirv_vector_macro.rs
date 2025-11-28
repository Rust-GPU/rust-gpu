// build-pass
// only-vulkan1.2
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformShuffleRelative,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""

use spirv_std::arch::subgroup_shuffle_up;
use spirv_std::glam::Vec3;
use spirv_std::spirv;

#[spirv_std::spirv_vector]
#[derive(Copy, Clone, Default)]
pub struct MyColor {
    pub r: f32,
    pub g: f32,
    pub b: f32,
}

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &Vec3,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut MyColor,
) {
    let color = MyColor {
        r: input.x,
        g: input.y,
        b: input.z,
    };
    // any function that accepts a `VectorOrScalar` would do
    *output = subgroup_shuffle_up(color, 5);
}
