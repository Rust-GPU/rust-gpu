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

#[spirv(vertex)]
pub fn vertex(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] buffer_positions: &[Vec4],
    #[spirv(vertex_index)] vertex_index: u32,
    #[spirv(position)] position_out: &mut Vec4,
) {
    *position_out = buffer_positions[vertex_index as usize];
}
