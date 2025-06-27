// Test that constant integer casts are optimized to avoid creating intermediate types
// that would require additional capabilities (e.g., Int8 capability for u8).

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

use spirv_std::spirv;

const K: u8 = 20;

#[spirv(fragment)]
pub fn main(output: &mut u32) {
    let position = 2u32;
    // This cast should be optimized to directly create a u32 constant with value 20,
    // avoiding the creation of a u8 type that would require Int8 capability
    let global_y_offset_bits = position * K as u32;
    *output = global_y_offset_bits;
}
