// build-pass
// compile-flags: -C target-feature=+RuntimeDescriptorArray,+ext:SPV_EXT_descriptor_indexing
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

use glam::Vec4;
use spirv_std::spirv;
use spirv_std::{RuntimeArray, TypedBuffer};

#[spirv(fragment)]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] desc_array: &RuntimeArray<
        TypedBuffer<Vec4>,
    >,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] desc_array_mut: &mut RuntimeArray<
        TypedBuffer<Vec4>,
    >,
) {
    unsafe {
        for buffer in 0..3 {
            let mut buffer_from = desc_array.index(buffer);
            let buffer_to = desc_array_mut.index_mut(buffer);
            **buffer_to = **buffer_from;
        }
    }
}
