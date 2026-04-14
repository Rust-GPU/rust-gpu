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
pub struct A(UVec3);

#[repr(C)]
#[derive(Default)]
pub struct B(A);

#[repr(transparent)]
#[derive(Default)]
pub struct AT(UVec3);

#[repr(transparent)]
#[derive(Default)]
pub struct BT(AT);

#[spirv(vertex)]
pub fn main(a: &mut B, at: &mut BT, #[spirv(local_invocation_index)] tid: u32) {
    *a = B(A(UVec3::new(tid, 1, 2)));
    a.0.0.y = tid + 1;
    a.0.0.x += 4;
    *at = BT(AT(UVec3::new(tid, 1, 2)));
    at.0.0.y = tid + 2;
    at.0.0.x += 5;
}
