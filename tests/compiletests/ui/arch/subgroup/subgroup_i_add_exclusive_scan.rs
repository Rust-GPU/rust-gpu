// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformArithmetic,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_i_add_exclusive_scan::subgroup_i_add_exclusive_scan

use glam::UVec3;
use spirv_std::arch::{GroupOperation, SubgroupMask};
use spirv_std::spirv;

unsafe fn subgroup_i_add_exclusive_scan(value: u32) -> u32 {
    spirv_std::arch::subgroup_exclusive_i_add(value)
}

#[spirv(compute(threads(32, 1, 1)))]
pub fn main(#[spirv(local_invocation_id)] local_invocation_id: UVec3) {
    unsafe {
        subgroup_i_add_exclusive_scan(local_invocation_id.x);
    }
}
