#[cfg(target_arch = "spirv")]
use core::arch::asm;
use spirv_std::memory::{Scope, Semantics};

/// See [`spirv_std::arch::control_barrier`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpControlBarrier")]
#[inline]
pub fn control_barrier<const EXECUTION: Scope, const MEMORY: Scope, const SEMANTICS: Semantics>() {
    const { SEMANTICS.assert_valid() }
    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%execution = OpConstant %u32 {execution}",
            "%memory = OpConstant %u32 {memory}",
            "%semantics = OpConstant %u32 {semantics}",
            "OpControlBarrier %execution %memory %semantics",
            execution = const EXECUTION as u32,
            memory = const MEMORY as u32,
            semantics = const SEMANTICS.bits(),
        }
    }
}

/// See [`spirv_std::arch::memory_barrier`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpMemoryBarrier")]
#[inline]
pub fn memory_barrier<const MEMORY: Scope, const SEMANTICS: Semantics>() {
    const { SEMANTICS.assert_valid() }
    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%memory = OpConstant %u32 {memory}",
            "%semantics = OpConstant %u32 {semantics}",
            "OpMemoryBarrier %memory %semantics",
            memory = const MEMORY as u32,
            semantics = const SEMANTICS.bits(),
        }
    }
}
