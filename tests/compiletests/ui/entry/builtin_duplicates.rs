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
use spirv_std::{glam::*, spirv};

#[spirv(compute(threads(1)))]
pub fn entry_1(#[spirv(local_invocation_index)] _local_idx: u32) {
    let _: u32 = local_invocation_index();
    let _: u32 = local_invocation_index();
    let _: u32 = sub_1();
    let _: u32 = sub_2();
}

#[spirv(compute(threads(1)))]
pub fn entry_2(#[spirv(local_invocation_index)] _local_idx: u32) {
    let _: u32 = local_invocation_index();
    let _: u32 = sub_1();
    let _: u32 = sub_2();
}

#[inline(never)]
fn sub_1() -> u32 {
    local_invocation_index()
}

#[inline(never)]
fn sub_2() -> u32 {
    local_invocation_index()
}

#[inline]
pub fn local_invocation_index() -> u32 {
    unsafe {
        let result = 0;
        asm! {
            "%builtin = OpVariable typeof{result} Input",
            "OpDecorate %builtin BuiltIn LocalInvocationIndex",
            "%result = OpLoad typeof*{result} %builtin",
            "OpStore {result} %result",
            result = in(reg) &result,
        }
        result
    }
}
