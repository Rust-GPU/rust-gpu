#![crate_name = "spec_constant_attr"]

// Tests the various forms of `#[spirv(spec_constant)]`.

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(spec_constant(id = 1))] no_default: u32,
    #[spirv(spec_constant(id = 2, default = 0))] default_0: u32,
    #[spirv(spec_constant(id = 123, default = 123))] default_123: u32,
    #[spirv(spec_constant(id = 0xffff_ffff, default = 0xffff_ffff))] max_id_and_default: u32,

    out: &mut u32,
) {
    *out = no_default + default_0 + default_123 + max_id_and_default;
}
