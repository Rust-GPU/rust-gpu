//! Traits related to scalars.

use crate::vector::{VectorOrScalar, create_dim};
use core::num::NonZeroUsize;

/// Abstract trait representing a SPIR-V scalar type.
///
/// # Safety
/// Implementing this trait on non-scalar types breaks assumptions of other unsafe code, and should
/// not be done.
pub unsafe trait Scalar: VectorOrScalar<Scalar = Self> + crate::sealed::Sealed {}

macro_rules! impl_scalar {
    ($($ty:ty),+) => {
        $(
            unsafe impl VectorOrScalar for $ty {
                type Scalar = Self;
                const DIM: NonZeroUsize = create_dim(1);
            }
            unsafe impl Scalar for $ty {}
        )+
    };
}

impl_scalar!(bool, f32, f64, u8, u16, u32, u64, i8, i16, i32, i64);
