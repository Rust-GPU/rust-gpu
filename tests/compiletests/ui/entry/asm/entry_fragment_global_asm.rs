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

pub fn main() {
    spirv_std::arch::kill()
}

pub fn entry() {
    main();
}

pub fn non_global_asm() {
    unsafe {
        asm! {
            "OpEntryPoint Fragment {entry} \"main\"",
            "OpExecutionMode {entry} OriginUpperLeft",
            entry = in(reg) entry,
        }
    }
}
