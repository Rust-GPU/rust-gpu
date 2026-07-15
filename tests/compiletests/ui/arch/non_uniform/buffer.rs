// build-pass
// compile-flags: -C llvm-args=--disassemble
// compile-flags: -C target-feature=+RuntimeDescriptorArray,+StorageBufferArrayDynamicIndexing
// compile-flags: -C target-feature=+ShaderNonUniform,+StorageBufferArrayNonUniformIndexing
// normalize-stderr-test "\n\W*OpSource .*" -> ""
// normalize-stderr-test "\n\W*OpLine .*" -> ""
// normalize-stderr-test "\n\W*%\d+ = OpString .*" -> ""
// normalize-stderr-test "\n\W*; .*" -> ""
// normalize-stderr-test "\n\W*OpCapability VulkanMemoryModel" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-spv1.4
// ignore-vulkan1.0
// ignore-vulkan1.1

use spirv_std::glam::*;
use spirv_std::image::*;
use spirv_std::*;

#[spirv(vertex)]
pub fn main(
    index: u32,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffers: &RuntimeArray<
        TypedBuffer<Vec4>,
    >,
    out: &mut Vec4,
) {
    unsafe {
        *out = **buffers.index(index as usize);
    }
}
