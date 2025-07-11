// Test that constant narrowing casts (e.g., u32 to u8) still work correctly
// and produce the expected truncation behavior.

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(output: &mut u32) {
    // This should create a u32 type and do proper truncation
    const BIG: u32 = 300; // 0x12C
    let truncated = BIG as u8; // Should be 0x2C = 44
    *output = truncated as u32;

    // This should optimize away the u8 type since it's widening
    const SMALL: u8 = 20;
    let widened = SMALL as u32;
    *output += widened;
}
