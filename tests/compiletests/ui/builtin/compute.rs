// build-pass
// compile-flags: -C llvm-args=--disassemble
// compile-flags: -C target-feature=+GroupNonUniform
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// normalize-stderr-test "; .*\n" -> ""
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-vulkan1.0
// ignore-vulkan1.1

use spirv_std::{
    builtin::compute,
    glam::*,
    spirv,
};

#[spirv(compute(threads(1)))]
pub fn compute(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut u32,
) {
    // Local ID's
    let _local_invocation_id: UVec3 = compute::local_invocation_id();
    let local_invocation_index: u32 = compute::local_invocation_index();

    // Global ID's
    let _global_invocation_id: UVec3 = compute::global_invocation_id();

    // Subgroup ID's
    let _num_subgroups: u32 = compute::num_subgroups();
    let _subgroup_id: u32 = compute::subgroup_id();
    let _subgroup_local_invocation_index: u32 = compute::subgroup_local_invocation_id();
    let _subgroup_size: u32 = compute::subgroup_size();

    // Workgroup ID's
    let _num_workgroups: UVec3 = compute::num_workgroups();
    let _workgroup_id: UVec3 = compute::workgroup_id();

    *out = local_invocation_index;
}
