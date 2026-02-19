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
use spirv_std::TypedBuffer;
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

        let mut result_slot = core::mem::MaybeUninit::<&TypedBuffer<Vec2>>::uninit();
        asm!(
            "%var = OpVariable typeof*{result_slot} StorageBuffer",
            "OpDecorate %var DescriptorSet {descriptor_set}",
            "OpDecorate %var Binding {binding}",
            "OpName %var \"viewport_size\"",
            "OpStore {result_slot} %var",
            descriptor_set = const 0,
            binding = const 0,
            result_slot = in(reg) result_slot.as_mut_ptr(),
        );
        let viewport_size_buffer: &TypedBuffer<Vec2> = result_slot.assume_init();
        let viewport_size: &Vec2 = &**viewport_size_buffer;

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
