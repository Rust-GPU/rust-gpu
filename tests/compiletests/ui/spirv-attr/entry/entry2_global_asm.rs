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
pub fn main(frag_coord: Vec4, viewport_size: &Vec2, output: &mut Vec4) {
    *output = Vec4::from((frag_coord.xy() / *viewport_size, 0., 1.));
}

pub fn entry() {
    unsafe {
        let mut frag_coord = Vec4::default();
        asm! {
            "%frag_coord = OpVariable typeof{frag_coord} Input",
            "OpDecorate %frag_coord BuiltIn FragCoord",
            "OpName %frag_coord \"frag_coord\"",
            "%tmp = OpLoad typeof*{frag_coord} %frag_coord",
            "OpStore {frag_coord} %tmp",
            frag_coord = in(reg) &mut frag_coord,
        };

        // FAILURE: rustc sees this struct as `#[repr(transparent)]` and "inlines" the member, removing the struct.
        // We can't declare structs in asm either atm, so kinda stuck.
        let mut viewport_size = &Vec2::default();
        asm! {
            "%block = OpTypeStruct typeof**{buffer}",
            "OpDecorate %block Block",
            "%ptr_block = OpTypePointer Generic %block",
            "%variable = OpVariable %ptr_block Input",
            "OpDecorate %variable DescriptorSet 0",
            "OpDecorate %variable Binding 0",
            "OpDecorate %variable NonWritable",
            "OpName %variable \"viewport_size\"",
            "%u32 = OpTypeInt 32 0",
            "%u32_0 = OpConstant %u32 0",
            "%ptr_content = OpTypePointer Generic typeof**{buffer}",
            "%block_value = OpInBoundsAccessChain %ptr_content %variable %u32_0",
            "%tmp = OpLoad typeof*{buffer} %block_value",
            "OpStore {buffer} %tmp",
            buffer = in(reg) &mut viewport_size,
        };

        let mut output = Vec4::default();
        main(frag_coord, viewport_size, &mut output);
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
            "OpEntryPoint Fragment {entry} \"main\"",
            "OpExecutionMode {entry} OriginUpperLeft",
            entry = in(reg) entry,
        }
    }
}
