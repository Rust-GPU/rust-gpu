// build-pass
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// normalize-stderr-test "; .*\n" -> ""

// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-vulkan1.0
// ignore-vulkan1.1

use core::arch::asm;
use spirv_std::compute::local_invocation_index;
use spirv_std::{glam::*, spirv};

#[spirv(compute(threads(1)))]
pub fn entry_1() {
    let _: u32 = local_invocation_index();
    let _: u32 = local2();
    let _: u32 = local3();
}

#[inline]
pub fn local2() -> u32 {
    spirv_std::load_builtin!(LocalInvocationIndex)
}

#[inline(never)]
pub fn local3() -> u32 {
    spirv_std::load_builtin!(LocalInvocationIndex)
}
