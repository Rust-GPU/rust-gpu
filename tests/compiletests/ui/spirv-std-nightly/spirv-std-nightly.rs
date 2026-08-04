// build-pass
// compile-flags: -C target-feature=+VulkanMemoryModelDeviceScopeKHR,+ext:SPV_KHR_vulkan_memory_model

#![feature(adt_const_params)]

use spirv_std_nightly::arch::atomic_i_add;
use spirv_std_nightly::arch::memory_barrier;
use spirv_std_nightly::memory::{Scope, Semantics};
use spirv_std_nightly::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(storage_buffer, descriptor_set = 0, binding = 0)] atomic_place: &mut u32) {
    unsafe {
        memory_barrier::<
            { Scope::Subgroup },
            {
                Semantics::ACQUIRE
                    .union(Semantics::IMAGE_MEMORY)
                    .union(Semantics::UNIFORM_MEMORY)
            },
        >();
        atomic_i_add::<_, { Scope::Device }, { Semantics::NONE }>(atomic_place, 12);
    }
}
