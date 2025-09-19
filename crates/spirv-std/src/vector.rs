//! Traits related to vectors.

use crate::scalar::Scalar;
use core::num::NonZeroUsize;
use glam::{Vec3Swizzles, Vec4Swizzles};

/// Abstract trait representing either a vector or a scalar type.
///
/// # Safety
/// Implementing this trait on non-scalar or non-vector types may break assumptions about other
/// unsafe code, and should not be done.
pub unsafe trait VectorOrScalar: Copy + Default + Send + Sync + 'static {
    /// Either the scalar component type of the vector or the scalar itself.
    type Scalar: Scalar;

    /// The dimension of the vector, or 1 if it is a scalar
    const DIM: NonZeroUsize;
}

/// replace with `NonZeroUsize::new(n).unwrap()` once `unwrap()` is const stabilized
pub(crate) const fn create_dim(n: usize) -> NonZeroUsize {
    match NonZeroUsize::new(n) {
        None => panic!("dim must not be 0"),
        Some(n) => n,
    }
}

/// Abstract trait representing a SPIR-V vector type.
///
/// # Safety
/// Implementing this trait on non-simd-vector types breaks assumptions of other unsafe code, and
/// should not be done.
pub unsafe trait Vector<T: Scalar, const N: usize>: VectorOrScalar<Scalar = T> {}

macro_rules! impl_vector {
    ($($scalar:ty: $($vec:ty => $dim:literal),+;)+) => {
        $($(
            unsafe impl VectorOrScalar for $vec {
                type Scalar = $scalar;
                const DIM: NonZeroUsize = create_dim($dim);
            }
            unsafe impl Vector<$scalar, $dim> for $vec {}
        )+)+
    };
}

impl_vector! {
    f32: glam::Vec2 => 2, glam::Vec3 => 3, glam::Vec3A => 3, glam::Vec4 => 4;
    f64: glam::DVec2 => 2, glam::DVec3 => 3, glam::DVec4 => 4;
    u32: glam::UVec2 => 2, glam::UVec3 => 3, glam::UVec4 => 4;
    i32: glam::IVec2 => 2, glam::IVec3 => 3, glam::IVec4 => 4;
    bool: glam::BVec2 => 2, glam::BVec3 => 3, glam::BVec4 => 4;
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
