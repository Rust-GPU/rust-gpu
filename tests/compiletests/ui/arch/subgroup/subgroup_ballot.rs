// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformBallot,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_ballot::subgroup_ballot

use spirv_std::spirv;

unsafe fn subgroup_ballot(predicate: bool) -> bool {
    let ballot = spirv_std::arch::subgroup_ballot(predicate);
    spirv_std::arch::subgroup_inverse_ballot(ballot)
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        subgroup_ballot(true);
    }
}
