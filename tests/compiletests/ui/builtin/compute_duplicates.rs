// build-pass
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
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
pub fn compute(
    // TODO: Compile error: duplicate builtin
    #[spirv(local_invocation_index)] local_idx: u32,
) {
    let _local_invocation_index: u32 = compute::local_invocation_index();
    let _local_invocation_index: u32 = compute::local_invocation_index();
    let _local_invocation_index: u32 = f();
}

#[inline(never)]
fn f() -> u32 {
    compute::local_invocation_index()
}
