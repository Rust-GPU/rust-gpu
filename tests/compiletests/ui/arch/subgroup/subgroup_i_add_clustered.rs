// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformArithmetic,+GroupNonUniformClustered,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_i_add_clustered::subgroup_i_add_clustered

use glam::UVec3;
use spirv_std::arch::{GroupOperation, SubgroupMask};
use spirv_std::spirv;

unsafe fn subgroup_i_add_clustered(value: u32) -> u32 {
    spirv_std::arch::subgroup_clustered_i_add::<8, _>(value)
}

#[spirv(compute(threads(32, 1, 1)))]
pub fn main(#[spirv(local_invocation_id)] local_invocation_id: UVec3) {
    unsafe {
        subgroup_i_add_clustered(local_invocation_id.x);
    }
}
