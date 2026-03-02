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

use spirv_std::glam::*;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn compute(
    output: &mut Vec4,
    #[spirv(front_facing)] front_facing: bool,
    #[spirv(helper_invocation)] is_helper_invocation_builtin: bool,
    #[spirv(frag_coord)] frag_coord: Vec4,
    #[spirv(point_coord)] point_coord: Vec2,
    #[spirv(sample_position)] sample_position: Vec2,
    #[spirv(clip_distance)] clip_distance: [f32; 2],
    #[spirv(cull_distance)] cull_distance: [f32; 2],
    #[spirv(flat, primitive_id)] primitive_index: u32,
    #[spirv(flat, sample_id)] sample_index: u32,
    #[spirv(flat, sample_mask)] sample_mask: [u32; 2],
    #[spirv(flat, layer)] layer: u32,
    #[spirv(flat, viewport_index)] viewport_index: u32,
) {
    let front_facing = if front_facing { 1. } else { 0. };
    let is_helper_invocation_builtin = if is_helper_invocation_builtin { 1. } else { 0. };
    *output = frag_coord
        + Vec4::from((
            point_coord + sample_position,
            front_facing + is_helper_invocation_builtin + clip_distance[0] + cull_distance[0],
            (primitive_index
                + sample_index
                + sample_mask[0]
                + layer
                + viewport_index) as f32,
        ));
}
