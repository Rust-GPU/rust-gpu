//! Traits related to scalars.

use crate::vector::VectorOrScalar;
use core::num::NonZeroUsize;

/// Abstract trait representing a SPIR-V scalar type.
///
/// Implemented on types that map to spirv "scalar" types, which includes:
/// * Floating-point type: f32, f64
/// * Integer type: u8, u16, u32, u64, i8, i16, i32, i64
/// * Boolean type: bool
///
/// See the SPIRV spec on [Types](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_types).
///
/// # Safety
/// Must only be implemented on spirv "scalar" types, as mentioned above.
pub unsafe trait Scalar: VectorOrScalar<Scalar = Self> + crate::sealed::Sealed {}

macro_rules! impl_scalar {
    ($($ty:ty),+) => {
        $(
            unsafe impl VectorOrScalar for $ty {
                type Scalar = Self;
                const DIM: NonZeroUsize = NonZeroUsize::new(1).unwrap();
            }
            unsafe impl Scalar for $ty {}
        )+
    };
}

impl_scalar!(bool, f32, f64, u8, u16, u32, u64, i8, i16, i32, i64);
