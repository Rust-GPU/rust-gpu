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

pub fn add_one(a: Vec4) -> Vec4 {
    a + 1.
}

#[spirv(fragment)]
pub fn main(a: Vec4, result: &mut Vec4) {
    unsafe {
        asm! {
            "%f32 = OpTypeFloat 32",
            "%a = OpLoad _ {a}",
            "%result = OpFunctionCall typeof*{result} {func} %a",
            "OpStore {result} %result",
            func = in(reg) add_one,
            a = in(reg) &a,
            result = in(reg) result,
        }
    }
}
