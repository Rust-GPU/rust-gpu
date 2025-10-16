// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformArithmetic,+GroupNonUniformClustered,+ext:SPV_KHR_vulkan_memory_model
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use glam::UVec3;
use spirv_std::arch::{GroupOperation, SubgroupMask};
use spirv_std::spirv;

unsafe fn subgroup_test_fail(value: u32) -> u32 {
    spirv_std::arch::subgroup_clustered_i_add::<0, _>(value)
}

#[spirv(compute(threads(32, 1, 1)))]
pub fn main(#[spirv(local_invocation_id)] local_invocation_id: UVec3) {
    unsafe {
        subgroup_test_fail(local_invocation_id.x);
    }
}
