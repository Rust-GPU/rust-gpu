// build-pass
// compile-flags: -C llvm-args=--disassemble
// compile-flags: -C target-feature=+Int8
// compile-flags: -C target-feature=+Int16
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-vulkan1.0
// ignore-vulkan1.1

use spirv_std::ExplicitLayout;
use spirv_std::glam::*;
use spirv_std::prototype::MyStruct;
use spirv_std::{Image, spirv};

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] input: &[u32],
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] output: &mut [u32],
) {
    let x = MyStruct::read(input, 0);
    MyStruct::write(output, 0, x);
}
