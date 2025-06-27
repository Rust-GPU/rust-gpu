// Test whether float constant casts need optimization

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(output: &mut f32) {
    // Test f64 to f32 (narrowing)
    const BIG: f64 = 123.456;
    let narrowed = BIG as f32;
    *output = narrowed;

    // Test f32 to f64 (widening) - this might create f32 type unnecessarily
    const SMALL: f32 = 20.5;
    let widened = SMALL as f64;
    *output += widened as f32;

    let kept: f32 = 1.0 + SMALL;
    *output += kept;

    // Test integer to float
    const INT: u32 = 42;
    let as_float = INT as f32;
    *output += as_float;
}
