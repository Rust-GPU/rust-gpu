// build-pass
// compile-flags: -C llvm-args=--disassemble

// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// normalize-stderr-test "; .*\n" -> ""

use spirv_std::{
    builtin::compute,
    glam::*,
    spirv,
};

#[spirv(compute(threads(1)))]
pub fn entry_1(
    #[spirv(local_invocation_index)] _local_idx: u32,
) {
    let _: u32 = compute::local_invocation_index();
    let _: u32 = compute::local_invocation_index();
    let _: u32 = sub_1();
    let _: u32 = sub_2();
}

#[spirv(compute(threads(1)))]
pub fn entry_2(
    #[spirv(local_invocation_index)] _local_idx: u32,
) {
    let _: u32 = compute::local_invocation_index();
    let _: u32 = sub_1();
    let _: u32 = sub_2();
}

#[inline(never)]
fn sub_1() -> u32 {
    compute::local_invocation_index()
}

#[inline(never)]
fn sub_2() -> u32 {
    compute::local_invocation_index()
}
