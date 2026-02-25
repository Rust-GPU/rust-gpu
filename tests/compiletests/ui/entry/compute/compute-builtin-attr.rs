// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
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

#[spirv(compute(threads(1)))]
pub fn compute(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] buffer: &mut [u32],
    #[spirv(local_invocation_id)] local_invocation_id: UVec3,
    #[spirv(local_invocation_index)] local_invocation_index: u32,
    #[spirv(global_invocation_id)] global_invocation_id: UVec3,
    #[spirv(num_workgroups)] num_workgroups: UVec3,
    #[spirv(workgroup_id)] workgroup_id: UVec3,
) {
    buffer[0] = local_invocation_id.x
        + local_invocation_index
        + global_invocation_id.x
        + num_workgroups.x
        + workgroup_id.x;
}
