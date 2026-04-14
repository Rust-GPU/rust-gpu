// build-pass
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "^(; .*\n)*" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-vulkan1.0
// ignore-vulkan1.1

use core::marker::PhantomData;
use spirv_std::glam::*;
use spirv_std::spirv;

#[repr(C)]
#[derive(Default)]
pub struct A(u32);

#[repr(transparent)]
#[derive(Default)]
pub struct AT(u32);

#[spirv(vertex)]
pub fn main(a: &mut A, at: &mut AT, #[spirv(local_invocation_index)] tid: u32) {
    *a = A(tid);
    a.0 = tid + 1;
    a.0 += 4;
    *at = AT(tid);
    at.0 = tid + 2;
    at.0 += 5;
}
