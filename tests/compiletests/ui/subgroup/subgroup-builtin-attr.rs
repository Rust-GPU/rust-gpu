// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformBallot
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-vulkan1.0
// ignore-vulkan1.1
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3

use spirv_std::glam::*;
use spirv_std::spirv;
use spirv_std::subgroup::*;

#[spirv(compute(threads(1)))]
pub fn compute(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] buffer: &mut [u32],
    #[spirv(num_subgroups)] num_subgroups: u32,
    #[spirv(subgroup_id)] subgroup_id: u32,
    #[spirv(subgroup_size)] subgroup_size: u32,
    // spirv name differs!
    #[spirv(subgroup_local_invocation_id)] subgroup_invocation_id: u32,
    #[spirv(subgroup_eq_mask)] subgroup_eq_mask: SubgroupMask,
    #[spirv(subgroup_ge_mask)] subgroup_ge_mask: SubgroupMask,
    #[spirv(subgroup_gt_mask)] subgroup_gt_mask: SubgroupMask,
    #[spirv(subgroup_le_mask)] subgroup_le_mask: SubgroupMask,
    #[spirv(subgroup_lt_mask)] subgroup_lt_mask: SubgroupMask,
) {
    buffer[0] = num_subgroups + subgroup_id + subgroup_size + subgroup_invocation_id;
    buffer[1] = subgroup_eq_mask.x
        + subgroup_ge_mask.x
        + subgroup_gt_mask.x
        + subgroup_le_mask.x
        + subgroup_lt_mask.x;
}
