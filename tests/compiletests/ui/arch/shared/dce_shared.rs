// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-vulkan1.0
// ignore-vulkan1.1

use spirv_std::arch::workgroup_memory_barrier_with_group_sync;
use spirv_std::glam::*;
use spirv_std::spirv;

#[spirv(compute(threads(2)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] input: &f32,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] output: &mut f32,
    #[spirv(workgroup)] used_shared: &mut f32,
    #[spirv(workgroup)] dce_shared: &mut [i32; 2],
    #[spirv(local_invocation_index)] inv_id: UVec3,
) {
    unsafe {
        let inv_id = inv_id.x as usize;
        if inv_id == 0 {
            *used_shared = *input;
        }
        workgroup_memory_barrier_with_group_sync();
        if inv_id == 1 {
            *output = *used_shared;
        }
    }
}
