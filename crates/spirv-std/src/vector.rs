//! Traits related to vectors.

use crate::sealed::Sealed;
use crate::{Scalar, ScalarOrVector};
use core::num::NonZeroUsize;
use glam::{Vec3Swizzles, Vec4Swizzles};

/// Abstract trait representing a SPIR-V vector type.
///
/// To derive this trait, mark your struct with:
/// ```no_run
/// #[spirv_std::spirv_vector]
/// # #[derive(Copy, Clone, Default)]
/// # struct Bla(f32, f32);
/// ```
///
/// This places these additional constraints on your type, checked by the spirv codegen:
/// * must be a struct
/// * members must be a spirv [`Scalar`] type, which includes:
///   * Floating-point type: f32, f64
///   * Integer type: u8, u16, u32, u64, i8, i16, i32, i64
///   * Boolean type: bool
/// * all members must be of the same primitive type
/// * must have 2, 3 or 4 vector components / members
/// * type must derive Copy, Clone, Default
///
/// The spirv codegen backend will then emit your type as an `OpTypeVector` instead of an `OpTypeStruct`. The layout of
/// your type is unaffected, the size, alignment and member offsets will follow standard rustc layout rules. This hint
/// does nothing on other target platforms.
///
/// See the SPIRV spec on [Types](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_types) and the
/// "Data rules" in the [Universal Validation Rules](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_universal_validation_rules).
///
/// # Example
/// ```no_run
/// #[spirv_std::spirv_vector]
/// #[derive(Copy, Clone, Default)]
/// struct MyColor {
///     r: f32,
///     b: f32,
///     g: f32,
/// }
/// ```
///
///
/// # Safety
/// * Must only be implemented on types that the spirv codegen emits as valid `OpTypeVector`. This includes all structs
///   marked with `#[rust_gpu::vector::v1]`, which `#[spirv_std::spirv_vector]` expands into or [`glam`]'s non-SIMD
/// "scalar" vector types use directly.
/// * `VectorOrScalar::DIM == N`, since const equality is behind rustc feature `associated_const_equality`
// Note(@firestar99) I would like to have these two generics be associated types instead. Doesn't make much sense for
// a vector type to implement this interface multiple times with different Scalar types or N, after all.
// While it's possible with `T: Scalar`, it's not with `const N: usize`, since some impl blocks in `image::params` need
// to be conditional on a specific N value. And you can only express that with const generics, but not with associated
// constants due to lack of const generics support in rustc.
pub unsafe trait Vector<T: Scalar, const N: usize>: ScalarOrVector<Scalar = T> {}

macro_rules! impl_vector {
    ($($ty:ty: [$scalar:ty; $n:literal];)+) => {
        $(
            impl Sealed for $ty {}
            unsafe impl ScalarOrVector for $ty {
                type Scalar = $scalar;
                const N: NonZeroUsize = NonZeroUsize::new($n).unwrap();
            }
            unsafe impl Vector<$scalar, $n> for $ty {}
        )+
    };
}

impl_vector! {
    glam::Vec2: [f32; 2];
    glam::Vec3: [f32; 3];
    glam::Vec3A: [f32; 3];
    glam::Vec4: [f32; 4];
    glam::DVec2: [f64; 2];
    glam::DVec3: [f64; 3];
    glam::DVec4: [f64; 4];
    glam::UVec2: [u32; 2];
    glam::UVec3: [u32; 3];
    glam::UVec4: [u32; 4];
    glam::IVec2: [i32; 2];
    glam::IVec3: [i32; 3];
    glam::IVec4: [i32; 4];
    glam::BVec2: [bool; 2];
    glam::BVec3: [bool; 3];
    glam::BVec4: [bool; 4];
}

/// Trait that implements slicing of a vector into a scalar or vector of lower dimensions, by
/// ignoring the higher dimensions
pub trait VectorTruncateInto<T> {
    /// Slices the vector into a lower dimensional type by ignoring the higher components
    fn truncate_into(self) -> T;
}

macro_rules! vec_trunc_impl {
    ($a:ty, $b:ty, $self:ident $(.$($e:tt)*)?) => {
        impl VectorTruncateInto<$a> for $b {
            fn truncate_into($self) -> $a {
                $self $(. $($e)*)?
            }
        }
    };
}
macro_rules! vec_trunc_impls {
    ($s:ty, $v2:ty, $v3:ty, $v4:ty) => {
        vec_trunc_impl! {$s, $s, self}
        vec_trunc_impl! {$s, $v2, self.x}
        vec_trunc_impl! {$s, $v3, self.x}
        vec_trunc_impl! {$s, $v4, self.x}

        vec_trunc_impl! {$v2, $v2, self}
        vec_trunc_impl! {$v2, $v3, self.xy()}
        vec_trunc_impl! {$v2, $v4, self.xy()}

        vec_trunc_impl! {$v3, $v3, self}
        vec_trunc_impl! {$v3, $v4, self.xyz()}

        vec_trunc_impl! {$v4, $v4, self}
    };
}

vec_trunc_impls! { f32, glam::Vec2, glam::Vec3, glam::Vec4 }
vec_trunc_impls! { f64, glam::DVec2, glam::DVec3, glam::DVec4 }
vec_trunc_impls! { i32, glam::IVec2, glam::IVec3, glam::IVec4 }
vec_trunc_impls! { u32, glam::UVec2, glam::UVec3, glam::UVec4 }
