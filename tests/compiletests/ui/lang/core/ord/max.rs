// build-pass
// compile-flags: -C llvm-args=--spirt-passes=reduce
// compile-flags: -C llvm-args=--disassemble
//
// FIXME(eddyb) consider using such replacements also for dealing
// with `OpLine` changing all the time (esp. in libcore functions).
//
// normalize-stderr-test "; (SPIR-V|Generator: rspirv|Version: 1\.\d+|Bound: \d+)\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// FIXME(eddyb) handle this one in the test runner.
// normalize-stderr-test "\S*/lib/rustlib/" -> "$$SYSROOT/lib/rustlib/"

// HACK(eddyb) `compiletest` handles `ui\dis\`, but not `ui\\dis\\`, on Windows.
// normalize-stderr-test "ui/dis/" -> "$$DIR/"

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] a: u32, #[spirv(flat)] b: u32, output: &mut u32) {
    *output = a.max(b);
}
