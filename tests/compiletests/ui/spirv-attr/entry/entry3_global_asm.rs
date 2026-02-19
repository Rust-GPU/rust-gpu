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
use spirv_std::arch::IndexUnchecked;
use spirv_std::glam::*;
use spirv_std::spirv;

pub fn main(input: &TypedBuffer<[u32]>, output: &mut TypedBuffer<[u32]>, gid: UVec3) {
    unsafe {
        let gid = gid.x;
        *output.index_unchecked_mut(gid as usize) = *input.index_unchecked(gid as usize);
    }
}

pub fn entry() {
    unsafe {
        let mut input_slot = core::mem::MaybeUninit::<&TypedBuffer<[u32]>>::uninit();
        asm!(
            "%var = OpVariable typeof*{result_slot} StorageBuffer",
            "OpDecorate %var DescriptorSet {descriptor_set}",
            "OpDecorate %var Binding {binding}",
            "OpDecorate %var NonWritable",
            "OpName %var \"input\"",
            "OpStore {result_slot} %var",
            descriptor_set = const 0,
            binding = const 0,
            result_slot = in(reg) input_slot.as_mut_ptr(),
        );
        let input = input_slot.assume_init();

        let mut output_slot = core::mem::MaybeUninit::<&mut TypedBuffer<[u32]>>::uninit();
        asm!(
            "%var = OpVariable typeof*{result_slot} StorageBuffer",
            "OpDecorate %var DescriptorSet {descriptor_set}",
            "OpDecorate %var Binding {binding}",
            "OpName %var \"output\"",
            "OpStore {result_slot} %var",
            descriptor_set = const 0,
            binding = const 1,
            result_slot = in(reg) output_slot.as_mut_ptr(),
        );
        let output = output_slot.assume_init();

        let gid = UVec3::default();
        asm! {
            "%builtin = OpVariable typeof{result} Input",
            "OpDecorate %builtin BuiltIn GlobalInvocationId",
            "OpName %builtin \"gid\"",
            "%result = OpLoad typeof*{result} %builtin",
            "OpStore {result} %result",
            result = in(reg) &gid,
        }

        main(input, output, gid);
    }
}

/// we don't support `global_asm!` yet
pub fn non_global_asm() {
    unsafe {
        asm! {
            "OpEntryPoint GLCompute {entry} \"main\"",
            "OpExecutionMode {entry} LocalSize 32 1 1",
            entry = in(reg) entry,
        }
    }
}
