// build-pass
// compile-flags: -C llvm-args=--disassemble
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

use core::arch::asm;
use spirv_std::glam::*;
use spirv_std::spirv;

pub fn add_one(a: f32) -> f32 {
    a + 1.
}

#[spirv(fragment)]
pub fn main(a: f32, result: &mut f32) {
    unsafe {
        asm! {
            "%f32 = OpTypeFloat 32",
            "%result = OpFunctionCall %f32 {func} {a}",
            "OpStore {result} %result",
            func = in(reg) add_one,
            a = in(reg) a,
            result = in(reg) result,
        }
    }
}
