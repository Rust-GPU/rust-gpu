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
        #[derive(Copy, Clone, Default)]
        struct ViewportBuffer(Vec2);
        let mut viewport_size = &ViewportBuffer::default();
        asm! {
            "OpDecorate typeof**{viewport_size} Block",
            "%viewport_size = OpVariable typeof{viewport_size} Input",
            "OpDecorate %viewport_size DescriptorSet 0",
            "OpDecorate %viewport_size Binding 0",
            "OpDecorate %viewport_size NonWritable",
            "OpName %viewport_size \"viewport_size\"",
            "%tmp = OpLoad typeof*{viewport_size} %viewport_size",
            "OpStore {viewport_size} %tmp",
            viewport_size = in(reg) &mut viewport_size,
        };

        let mut output = Vec4::default();
        main(frag_coord, &viewport_size.0, &mut output);
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
