#![crate_name = "workgroup_size"]

// Tests that the WorkgroupSize builtin is correctly generated as a constant.

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

use spirv_std::glam::UVec3;
use spirv_std::spirv;

#[spirv(compute(threads(8, 4, 2)))]
pub fn main(#[spirv(workgroup_size)] size: UVec3, #[spirv(local_invocation_id)] local_id: UVec3) {
    // The workgroup_size should be (8, 4, 2)
    // Using the size parameter ensures it's included in the generated SPIR-V
    let _total = size.x + size.y + size.z + local_id.x;
}
