// build-fail
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// normalize-stderr-test "= note: module `.*`" -> "= note: module `<normalized>`"
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-spv1.4
// ignore-spv1.5
// ignore-spv1.6
// ignore-vulkan1.0
// ignore-vulkan1.1

use spirv_std::glam::*;
use spirv_std::{Image, spirv};

#[spirv(vertex)]
pub fn main(#[spirv(location = 0)] out1: &mut Mat4, #[spirv(location = 1)] out2: &mut Vec2) {
    *out1 = Default::default();
    *out2 = Default::default();
}
