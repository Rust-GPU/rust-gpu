// build-pass
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "\n\W*OpLine .*" -> ""
// normalize-stderr-test "\n\W*OpSource .*" -> ""
// normalize-stderr-test "\n\W*%\d+ = OpString .*" -> ""
// normalize-stderr-test "\n\W*OpCapability VulkanMemoryModel" -> ""
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

#[derive(Default)]
pub struct A(());
#[repr(transparent)]
#[derive(Default)]
pub struct AT(());

#[spirv(vertex)]
pub fn main(a: &mut A, at: &mut AT) {
    *a = A(());
    a.0 = ();
    *at = AT(());
    at.0 = ();
}
