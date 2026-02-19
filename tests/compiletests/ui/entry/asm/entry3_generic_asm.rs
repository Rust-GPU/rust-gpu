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

#[inline]
pub fn main(input: &TypedBuffer<[u32]>, output: &mut TypedBuffer<[u32]>, gid: UVec3) {
    unsafe {
        let gid = gid.x;
        *output.index_unchecked_mut(gid as usize) = *input.index_unchecked(gid as usize);
    }
}

pub fn entry() {
    let input = decl_buffer::<0, 0, [u32]>();
    let output = decl_buffer_mut::<0, 1, [u32]>();
    let gid = global_invocation_id();
    main(input, output, gid);
}

#[inline]
pub fn decl_buffer<const DESCRIPTOR_SET: u32, const BINDING: u32, T: ?Sized>()
-> &'static TypedBuffer<T> {
    unsafe {
        let mut slot = core::mem::MaybeUninit::<&TypedBuffer<T>>::uninit();
        asm!(
            "%var = OpVariable typeof*{slot} StorageBuffer",
            "OpDecorate %var DescriptorSet {descriptor_set}",
            "OpDecorate %var Binding {binding}",
            "OpDecorate %var NonWritable",
            "OpStore {slot} %var",
            descriptor_set = const DESCRIPTOR_SET,
            binding = const BINDING,
            slot = in(reg) slot.as_mut_ptr(),
        );
        slot.assume_init()
    }
}

#[inline]
pub fn decl_buffer_mut<const DESCRIPTOR_SET: u32, const BINDING: u32, T: ?Sized>()
-> &'static mut TypedBuffer<T> {
    unsafe {
        let mut slot = core::mem::MaybeUninit::<&mut TypedBuffer<T>>::uninit();
        asm!(
            "%var = OpVariable typeof*{slot} StorageBuffer",
            "OpDecorate %var DescriptorSet {descriptor_set}",
            "OpDecorate %var Binding {binding}",
            "OpStore {slot} %var",
            descriptor_set = const DESCRIPTOR_SET,
            binding = const BINDING,
            slot = in(reg) slot.as_mut_ptr(),
        );
        slot.assume_init()
    }
}

#[inline]
pub fn global_invocation_id() -> UVec3 {
    unsafe {
        let gid = UVec3::default();
        asm! {
            "%builtin = OpVariable typeof{result} Input",
            "OpDecorate %builtin BuiltIn GlobalInvocationId",
            "%result = OpLoad typeof*{result} %builtin",
            "OpStore {result} %result",
            result = in(reg) &gid,
        }
        gid
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
