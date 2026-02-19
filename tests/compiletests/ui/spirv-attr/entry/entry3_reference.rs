// build-pass
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-vulkan1.0
// ignore-vulkan1.1

use spirv_std::TypedBuffer;
use spirv_std::arch::IndexUnchecked;
use spirv_std::glam::*;
use spirv_std::spirv;

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] input: &TypedBuffer<[u32]>,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] output: &mut TypedBuffer<[u32]>,
    #[spirv(global_invocation_id)] gid: UVec3,
) {
    unsafe {
        let gid = gid.x;
        *output.index_unchecked_mut(gid as usize) = *input.index_unchecked(gid as usize);
    }
}
