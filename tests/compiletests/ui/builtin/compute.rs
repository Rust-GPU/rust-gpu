// build-pass
// compile-flags: -C llvm-args=--disassemble
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

use spirv_std::glam::*;
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn compute(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut u32,
    // #[spirv(global_invocation_id)] global_invocation_id: UVec3,
    // #[spirv(local_invocation_id)] local_invocation_id: UVec3,
    // #[spirv(subgroup_local_invocation_id)] subgroup_local_invocation_id: u32,
    // #[spirv(num_subgroups)] num_subgroups: u32,
    // #[spirv(num_workgroups)] num_workgroups: UVec3,
    // #[spirv(subgroup_id)] subgroup_id: u32,
    // #[spirv(workgroup_id)] workgroup_id: UVec3,
) {
    let local_invocation_id = spirv_std::builtin::compute::local_invocation_id();
    *out = local_invocation_id.x;
}
