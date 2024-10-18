// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformBallot,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_ballot_bit_count::subgroup_ballot_bit_count

use spirv_std::arch::{GroupOperation, SubgroupMask};
use spirv_std::spirv;

unsafe fn subgroup_ballot_bit_count(ballot: SubgroupMask) -> u32 {
    spirv_std::arch::subgroup_ballot_bit_count(ballot)
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        subgroup_ballot_bit_count(spirv_std::arch::subgroup_ballot(true));
    }
}
