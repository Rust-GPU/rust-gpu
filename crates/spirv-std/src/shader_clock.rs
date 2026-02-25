//! Intrinsics for reading the shader clock.
//!
//! GLSL: [`GL_EXT_shader_realtime_clock`](https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GL_EXT_shader_realtime_clock.txt)
//! SPIRV: [`SPV_KHR_shader_clock`](https://github.khronos.org/SPIRV-Registry/extensions/KHR/SPV_KHR_shader_clock.html)

#[cfg(target_arch = "spirv")]
use core::arch::asm;
use glam::UVec2;

/// Read from the shader clock with either the `Subgroup` or `Device` scope.
///
/// See: <https://github.khronos.org/SPIRV-Registry/extensions/KHR/SPV_KHR_shader_clock.html>
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReadClockKHR")]
pub fn read_clock_khr<const SCOPE: u32>() -> u64 {
    unsafe {
        let mut result: u64;

        asm! {
            "%uint = OpTypeInt 32 0",
            "%scope = OpConstant %uint {scope}",
            "{result} = OpReadClockKHR typeof*{result} %scope",
            result = out(reg) result,
            scope = const SCOPE,
        };

        result
    }
}

/// Like `read_clock_khr` but returns a vector to avoid requiring the `Int64`
/// capability. It returns a 'vector of two-components of 32-bit unsigned
/// integer type with the first component containing the 32 least significant
/// bits and the second component containing the 32 most significant bits.'
///
/// See:
/// <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_shader_clock.html>
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReadClockKHR")]
pub fn read_clock_uvec2_khr<const SCOPE: u32>() -> UVec2 {
    unsafe {
        let mut result = UVec2::default();

        asm! {
            "%uint = OpTypeInt 32 0",
            "%scope = OpConstant %uint {scope}",
            "%result = OpReadClockKHR typeof*{result} %scope",
            "OpStore {result} %result",
            result = in(reg) &mut result,
            scope = const SCOPE,
        };

        result
    }
}
