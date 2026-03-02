// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// compile-flags: -C target-feature=+SampleRateShading,+Geometry,+MultiViewport
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-vulkan1.0
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2

use spirv_std::fragment::*;
use spirv_std::glam::*;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn compute(output: &mut Vec4) {
    unsafe {
        let front_facing = if front_facing() { 1. } else { 0. };
        let is_helper_invocation_builtin = if is_helper_invocation_builtin() {
            1.
        } else {
            0.
        };
        *output = frag_coord()
            + Vec4::from((
                point_coord() + sample_position(),
                front_facing
                    + is_helper_invocation_builtin
                    + clip_distance::<2>()[0]
                    + cull_distance::<2>()[0],
                (primitive_index() + sample_index() + sample_mask::<2>()[0] + layer() + viewport_index())
                    as f32,
            ));
    }
}
