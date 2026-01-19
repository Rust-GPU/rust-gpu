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

#[inline]
pub fn main(input: Vec4, out: &mut Vec4) {
    *out = input + 1.;
}

pub fn entry() {
    unsafe {
        let mut input = Vec4::default();
        asm! {
            "%input = OpVariable typeof{input} Input",
            "OpName %input \"input\"",
            "OpDecorate %input Location 0",
            "%tmp = OpLoad typeof*{input} %input",
            "OpStore {input} %tmp",
            input = in(reg) &mut input,
        };
        let mut output = Vec4::default();
        main(input, &mut output);
        asm! {
            "%output = OpVariable typeof{output} Output",
            "OpName %output \"output\"",
            "OpDecorate %output Location 0",
            "%tmp = OpLoad _ {output}",
            "OpStore %output %tmp",
            output = in(reg) &output,
        };
    }
}

/// we don't support `global_asm!` yet
pub fn non_global_asm() {
    unsafe {
        asm! {
            "OpEntryPoint Vertex {entry} \"main\"",
            entry = in(reg) entry,
        }
    }
}
