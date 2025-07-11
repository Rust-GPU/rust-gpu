// Test that constant float widening casts are optimized to avoid creating
// the smaller float type when not needed elsewhere.

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(output: &mut f64) {
    // This should optimize away the f32 type since it's widening
    const SMALL: f32 = 20.5;
    let widened = SMALL as f64;
    *output = widened;
}
