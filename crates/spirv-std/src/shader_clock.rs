//! Intrinsics for reading the shader clock.
//!
//! GLSL: [`GL_EXT_shader_realtime_clock`](https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GL_EXT_shader_realtime_clock.txt)
//! SPIRV: [`SPV_KHR_shader_clock`](https://github.khronos.org/SPIRV-Registry/extensions/KHR/SPV_KHR_shader_clock.html)

#[cfg(target_arch = "spirv")]
use crate::memory::Scope;
#[cfg(target_arch = "spirv")]
use core::arch::asm;
use glam::UVec2;

/// The [`read_clock`] function returns a 64-bit value representing a real-time clock that is globally coherent by
/// all invocations on the GPU. See [`read_clock_uvec2`] for a variant that doesn't require 64-bit support.
///
/// The units of time are not defined for either of these operations and will wrap after exceeding the maximum value
/// representable in 64 bits. These functions serve as code motion barriers.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReadClockKHR")]
pub fn read_clock() -> u64 {
    unsafe {
        let mut result = Default::default();
        asm! {
            "%uint = OpTypeInt 32 0",
            "%scope = OpConstant %uint {scope}",
            "%result = OpReadClockKHR typeof*{result} %scope",
            "OpStore {result} %result",
            result = in(reg) &mut result,
            scope = const (Scope::Device as u32),
        };
        result
    }
}

/// [`read_clock_uvec2`] returns the same value encoded as a two-component vector of 32-bit unsigned integers with the
/// first component containing the 32 least significant bits and the second component containing the 32 most significant
/// bits. See [`read_clock`] for a variant that returns a single `u64`.
///
/// The units of time are not defined for either of these operations and will wrap after exceeding the maximum value
/// representable in 64 bits. These functions serve as code motion barriers.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReadClockKHR")]
pub fn read_clock_uvec2() -> UVec2 {
    unsafe {
        let mut result = Default::default();
        asm! {
            "%uint = OpTypeInt 32 0",
            "%scope = OpConstant %uint {scope}",
            "%result = OpReadClockKHR typeof*{result} %scope",
            "OpStore {result} %result",
            result = in(reg) &mut result,
            scope = const (Scope::Device as u32),
        };
        result
    }
}
