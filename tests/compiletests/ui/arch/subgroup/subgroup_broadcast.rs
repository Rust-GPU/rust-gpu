// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformBallot,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_broadcast::disassembly
// normalize-stderr-test "OpLine .*\n" -> ""
// ignore-vulkan1.0
// ignore-vulkan1.1
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-spv1.4

use spirv_std::arch::{GroupOperation, SubgroupMask};
use spirv_std::spirv;

unsafe fn disassembly(value: i32, id: u32) -> i32 {
    spirv_std::arch::subgroup_broadcast(value, id)
}

#[spirv(compute(threads(32, 1, 1)))]
pub fn main() {
    unsafe {
        disassembly(42, 5);
    }
}
