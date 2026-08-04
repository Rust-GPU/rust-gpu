mod atomics;
mod barrier;

// name overwritten functions explicitly to overrule glob import of the rest
pub use atomics::{
    atomic_compare_exchange, atomic_exchange, atomic_f_add, atomic_f_max, atomic_f_min,
    atomic_i_add, atomic_i_decrement, atomic_i_increment, atomic_i_sub, atomic_load, atomic_s_max,
    atomic_s_min, atomic_store, atomic_u_max, atomic_u_min,
};
pub use barrier::{control_barrier, memory_barrier};
pub use spirv_std::arch::*;

use crate::glam::UVec2;
use spirv_std::memory::Scope;

/// See [`spirv_std::arch::read_clock_khr`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReadClockKHR")]
pub fn read_clock_khr<const SCOPE: Scope>() -> u64 {
    spirv_std::arch::read_clock_khr::<{ SCOPE as u32 }>()
}

/// See [`spirv_std::arch::read_clock_uvec2_khr`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReadClockKHR")]
pub fn read_clock_uvec2_khr<const SCOPE: Scope>() -> UVec2 {
    spirv_std::arch::read_clock_uvec2_khr::<{ SCOPE as u32 }>()
}
