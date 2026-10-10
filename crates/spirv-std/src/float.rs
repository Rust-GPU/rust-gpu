//! Traits and helper functions related to floats.

use crate::glam::{Vec2, Vec4};
#[cfg(target_arch = "spirv")]
use core::arch::asm;
#[cfg(target_arch = "spirv")]
use core::intrinsics;

#[cfg(target_arch = "spirv")]
impl f32 {
    /// Returns the largest integer that is less than or equal to `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn floor(self) -> f32 {
        intrinsics::floorf32(self)
    }

    /// Returns the smallest integer that is greater than or equal to `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn ceil(self) -> f32 {
        intrinsics::ceilf32(self)
    }

    /// Returns the nearest integer to `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn round(self) -> f32 {
        intrinsics::roundf32(self)
    }

    /// Returns the nearest integer to a number.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn round_ties_even(self) -> f32 {
        intrinsics::round_ties_even_f32(self)
    }

    /// Returns the integer part of `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn trunc(self) -> f32 {
        intrinsics::truncf32(self)
    }

    /// Returns the fractional part of `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn fract(self) -> f32 {
        self - self.trunc()
    }

    /// Fused multiply-add.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn mul_add(self, a: f32, b: f32) -> f32 {
        intrinsics::fmaf32(self, a, b)
    }

    /// Calculates Euclidean division, the matching method for `rem_euclid`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn div_euclid(self, rhs: f32) -> f32 {
        let q = (self / rhs).trunc();
        if self % rhs < 0.0 {
            return if rhs > 0.0 { q - 1.0 } else { q + 1.0 };
        }
        q
    }

    /// Calculates the least nonnegative remainder of `self` when divided by `rhs`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn rem_euclid(self, rhs: f32) -> f32 {
        let r = self % rhs;
        if r < 0.0 { r + rhs.abs() } else { r }
    }

    /// Raises a number to an integer power.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn powi(self, n: i32) -> f32 {
        intrinsics::powif32(self, n)
    }

    /// Returns the square root of a number.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn sqrt(self) -> f32 {
        intrinsics::sqrtf32(self)
    }

    /// Returns the cube root of a number.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn cbrt(self) -> f32 {
        intrinsics::powf32(self, 1.0 / 3.0)
    }

    /// The positive difference of two numbers.
    #[inline]
    #[rustc_allow_incoherent_impl]
    #[deprecated(
        since = "0.10.0",
        note = "you probably meant `(self - other).abs()`: \
            this operation is `(self - other).max(0.0)` \
            except that `abs_sub` also propagates NaNs (also \
            known as `fdimf` in C). If you truly need the positive \
            difference, consider using that expression or the C function \
            `fdimf`, depending on how you wish to handle NaN (please consider \
            filing an issue describing your use-case too)."
    )]
    pub fn abs_sub(self, other: f32) -> f32 {
        let r = self - other;
        if r.is_nan() {
            r
        } else if r > 0.0 {
            r
        } else {
            0.0
        }
    }
}

#[cfg(target_arch = "spirv")]
impl f64 {
    /// Returns the largest integer that is less than or equal to `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn floor(self) -> f64 {
        intrinsics::floorf64(self)
    }

    /// Returns the smallest integer that is greater than or equal to `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn ceil(self) -> f64 {
        intrinsics::ceilf64(self)
    }

    /// Returns the nearest integer to `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn round(self) -> f64 {
        intrinsics::roundf64(self)
    }

    /// Returns the nearest integer to a number.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn round_ties_even(self) -> f64 {
        intrinsics::round_ties_even_f64(self)
    }

    /// Returns the integer part of `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn trunc(self) -> f64 {
        intrinsics::truncf64(self)
    }

    /// Returns the fractional part of `self`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn fract(self) -> f64 {
        self - self.trunc()
    }

    /// Fused multiply-add.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn mul_add(self, a: f64, b: f64) -> f64 {
        intrinsics::fmaf64(self, a, b)
    }

    /// Calculates Euclidean division, the matching method for `rem_euclid`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn div_euclid(self, rhs: f64) -> f64 {
        let q = (self / rhs).trunc();
        if self % rhs < 0.0 {
            return if rhs > 0.0 { q - 1.0 } else { q + 1.0 };
        }
        q
    }

    /// Calculates the least nonnegative remainder of `self` when divided by `rhs`.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn rem_euclid(self, rhs: f64) -> f64 {
        let r = self % rhs;
        if r < 0.0 { r + rhs.abs() } else { r }
    }

    /// Raises a number to an integer power.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn powi(self, n: i32) -> f64 {
        intrinsics::powif64(self, n)
    }

    /// Returns the square root of a number.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn sqrt(self) -> f64 {
        intrinsics::sqrtf64(self)
    }

    /// Returns the cube root of a number.
    #[inline]
    #[rustc_allow_incoherent_impl]
    pub fn cbrt(self) -> f64 {
        intrinsics::powf64(self, 1.0 / 3.0)
    }

    /// The positive difference of two numbers.
    #[inline]
    #[rustc_allow_incoherent_impl]
    #[deprecated(
        since = "0.10.0",
        note = "you probably meant `(self - other).abs()`: \
            this operation is `(self - other).max(0.0)` \
            except that `abs_sub` also propagates NaNs (also \
            known as `fdim` in C). If you truly need the positive \
            difference, consider using that expression or the C function \
            `fdim`, depending on how you wish to handle NaN (please consider \
            filing an issue describing your use-case too)."
    )]
    pub fn abs_sub(self, other: f64) -> f64 {
        let r = self - other;
        if r.is_nan() {
            r
        } else if r > 0.0 {
            r
        } else {
            0.0
        }
    }
}

/// Converts two f32 values (floats) into two f16 values (halfs). The result is a u32, with the low
/// 16 bits being the first f16, and the high 16 bits being the second f16.
#[spirv_std_macros::gpu_only]
pub fn vec2_to_f16x2(vec: Vec2) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 58 = PackHalf2x16
            "{result} = OpExtInst %uint %glsl 58 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Converts two f16 values (halfs) into two f32 values (floats). The parameter is a u32, with the
/// low 16 bits being the first f16, and the high 16 bits being the second f16.
#[spirv_std_macros::gpu_only]
pub fn f16x2_to_vec2(int: u32) -> Vec2 {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            // 62 = UnpackHalf2x16
            "%result = OpExtInst typeof*{result} %glsl 62 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}

/// Converts an f32 (float) into an f16 (half). The result is a u32, not a u16, due to GPU support
/// for u16 not being universal - the upper 16 bits will always be zero.
#[spirv_std_macros::gpu_only]
pub fn f32_to_f16(float: f32) -> u32 {
    vec2_to_f16x2(crate::glam::Vec2::new(float, 0.))
}

/// Converts an f16 (half) into an f32 (float). The parameter is a u32, due to GPU support for u16
/// not being universal - the upper 16 bits are ignored.
#[spirv_std_macros::gpu_only]
pub fn f16_to_f32(packed: u32) -> f32 {
    f16x2_to_vec2(packed).x
}

/// Packs a vec4 into 4 8-bit signed integers. See
/// [PackSnorm4x8](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn vec4_to_u8x4_snorm(vec: Vec4) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 54 = PackSnorm4x8
            "{result} = OpExtInst %uint %glsl 54 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Packs a vec4 into 4 8-bit unsigned integers. See
/// [PackUnorm4x8](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn vec4_to_u8x4_unorm(vec: Vec4) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 55 = PackUnorm4x8
            "{result} = OpExtInst %uint %glsl 55 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Packs a vec2 into 2 16-bit signed integers. See
/// [PackSnorm2x16](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn vec2_to_u16x2_snorm(vec: Vec2) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 56 = PackSnorm2x16
            "{result} = OpExtInst %uint %glsl 56 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Packs a vec2 into 2 16-bit unsigned integers. See
/// [PackUnorm2x16](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn vec2_to_u16x2_unorm(vec: Vec2) -> u32 {
    let result;
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%uint = OpTypeInt 32 0",
            "%vec = OpLoad _ {vec}",
            // 57 = PackUnorm2x16
            "{result} = OpExtInst %uint %glsl 57 %vec",
            vec = in(reg) &vec,
            result = out(reg) result,
        );
    }
    result
}

/// Unpacks 4 8-bit signed integers into a vec4. See
/// [UnpackSnorm4x8](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn u8x4_to_vec4_snorm(int: u32) -> Vec4 {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            // 63 = UnpackSnorm4x8
            "%result = OpExtInst typeof*{result} %glsl 63 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}

/// Unpacks 4 8-bit unsigned integers into a vec4. See
/// [UnpackSnorm4x8](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for exact
/// semantics.
#[spirv_std_macros::gpu_only]
pub fn u8x4_to_vec4_unorm(int: u32) -> Vec4 {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            // 64 = UnpackUnorm4x8
            "%result = OpExtInst typeof*{result} %glsl 64 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}

/// Unpacks 2 16-bit signed integers into a vec2. See
/// [UnpackSnorm2x16](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for
/// exact semantics.
#[spirv_std_macros::gpu_only]
pub fn u16x2_to_vec2_snorm(int: u32) -> Vec2 {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            // 60 = UnpackSnorm2x16
            "%result = OpExtInst typeof*{result} %glsl 60 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}

/// Unpacks 2 16-bit unsigned integers into a vec2. See
/// [UnpackUnorm2x16](https://www.khronos.org/registry/SPIR-V/specs/1.0/GLSL.std.450.html) for
/// exact semantics.
#[spirv_std_macros::gpu_only]
pub fn u16x2_to_vec2_unorm(int: u32) -> Vec2 {
    let mut result = Default::default();
    unsafe {
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            // 61 = UnpackUnorm2x16
            "%result = OpExtInst typeof*{result} %glsl 61 {int}",
            "OpStore {result} %result",
            int = in(reg) int,
            result = in(reg) &mut result,
        );
    }
    result
}
