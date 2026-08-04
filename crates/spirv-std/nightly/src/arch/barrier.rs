use spirv_std::memory::{Scope, Semantics};

/// See [`spirv_std::arch::control_barrier`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpControlBarrier")]
#[inline]
pub fn control_barrier<const EXECUTION: Scope, const MEMORY: Scope, const SEMANTICS: Semantics>() {
    const { SEMANTICS.assert_valid() }
    spirv_std::arch::control_barrier::<{ EXECUTION as u32 }, { MEMORY as u32 }, { SEMANTICS.bits() }>(
    )
}

/// See [`spirv_std::arch::memory_barrier`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpMemoryBarrier")]
#[inline]
pub fn memory_barrier<const MEMORY: Scope, const SEMANTICS: Semantics>() {
    const { SEMANTICS.assert_valid() }
    spirv_std::arch::memory_barrier::<{ MEMORY as u32 }, { SEMANTICS.bits() }>()
}
