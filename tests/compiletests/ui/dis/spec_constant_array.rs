// Tests the various forms of `#[spirv(spec_constant)]`.

// build-pass
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-vulkan1.0
// ignore-vulkan1.1

// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

// HACK(eddyb) `compiletest` handles `ui\dis\`, but not `ui\\dis\\`, on Windows.
// normalize-stderr-test "ui/dis/" -> "$$DIR/"

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(spec_constant(id = 42, default = 69))] array: [u32; 4],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] out: &mut u32,
) {
    *out = array[0] + array[1] + array[2] + array[3];
}
