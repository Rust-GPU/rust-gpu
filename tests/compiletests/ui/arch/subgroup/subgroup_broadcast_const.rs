// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformBallot,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_broadcast_const::disassembly
// normalize-stderr-test "OpLine .*\n" -> ""

use spirv_std::arch::{GroupOperation, SubgroupMask};
use spirv_std::spirv;

unsafe fn disassembly(value: i32) -> i32 {
    spirv_std::arch::subgroup_broadcast_const::<_, 5>(value)
}

#[spirv(compute(threads(32, 1, 1)))]
pub fn main() {
    unsafe {
        disassembly(-42);
    }
}
