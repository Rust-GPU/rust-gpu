//! Traits related to scalars.

use crate::VectorOrScalar;
use crate::sealed::Sealed;
use core::num::NonZeroUsize;

/// Abstract trait representing a SPIR-V scalar type, which includes:
/// * Floating-point type: f32, f64
/// * Integer type: u8, u16, u32, u64, i8, i16, i32, i64
/// * Boolean type: bool
///
/// See the SPIRV spec on [Types](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_types).
///
/// # Safety
/// Implementing this trait on non-scalar types breaks assumptions of other unsafe code, and should not be done.
pub unsafe trait Scalar: VectorOrScalar<Scalar = Self> + crate::sealed::Sealed {}

/// Abstract trait representing a SPIR-V integer or floating-point type. Unlike [`Scalar`], excludes the boolean type.
///
/// # Safety
/// Implementing this trait on non-primitive-integer or non-primitive-float types breaks assumptions of other unsafe
/// code, and should not be done.
pub unsafe trait Number: Scalar {}

/// Abstract trait representing any SPIR-V integer type.
///
/// # Safety
/// Implementing this trait on non-primitive-integer types breaks assumptions of other unsafe code,
/// and should not be done.
pub unsafe trait Integer: num_traits::PrimInt + Number {
    /// Width of the integer, in bits.
    const WIDTH: usize;
    /// If the integer is signed: true means signed, false means unsigned.
    const SIGNED: bool;
}

/// Abstract trait representing any SPIR-V signed integer type.
///
/// # Safety
/// Implementing this trait on non-signed-integer types breaks assumptions of other unsafe code,
/// and should not be done.
pub unsafe trait SignedInteger: num_traits::Signed + Integer {}

/// Abstract trait representing any SPIR-V unsigned integer type.
///
/// # Safety
/// Implementing this trait on non-unsigned-integer types breaks assumptions of other unsafe code,
/// and should not be done.
pub unsafe trait UnsignedInteger: num_traits::Unsigned + Integer {}

/// Abstract trait representing a SPIR-V floating point type.
///
/// # Safety
/// Implementing this trait on non-primitive-float types breaks assumptions of other unsafe code,
/// and should not be done.
pub unsafe trait Float: num_traits::Float + Number {
    /// Width of the float, in bits.
    const WIDTH: usize;
}

macro_rules! impl_scalar {
    (impl Scalar for $ty:ty;) => {
        impl Sealed for $ty {}
        unsafe impl VectorOrScalar for $ty {
            type Scalar = Self;
            const DIM: NonZeroUsize = NonZeroUsize::new(1).unwrap();
        }
        unsafe impl Scalar for $ty {}
    };
    (impl Number for $ty:ty;) => {
        unsafe impl Number for $ty {}
        impl_scalar!(impl Scalar for $ty;);
    };
    (impl UnsignedInteger for $ty:ty;) => {
        unsafe impl Integer for $ty {
            const WIDTH: usize = core::mem::size_of::<$ty>() * 8;
            const SIGNED: bool = false;
        }
        unsafe impl UnsignedInteger for $ty {}
        impl_scalar!(impl Number for $ty;);
    };
    (impl SignedInteger for $ty:ty;) => {
        unsafe impl Integer for $ty {
            const WIDTH: usize = core::mem::size_of::<$ty>() * 8;
            const SIGNED: bool = true;
        }
        unsafe impl SignedInteger for $ty {}
        impl_scalar!(impl Number for $ty;);
    };
    (impl Float for $ty:ty;) => {
        unsafe impl Float for $ty {
            const WIDTH: usize = core::mem::size_of::<$ty>() * 8;
        }
        impl_scalar!(impl Number for $ty;);
    };
    ($(impl $trait:ident for $ty:ty;)+) => {
        $(impl_scalar!(impl $trait for $ty;);)+
    };
}

impl_scalar! {
    impl UnsignedInteger for u8;
    impl UnsignedInteger for u16;
    impl UnsignedInteger for u32;
    impl UnsignedInteger for u64;
    impl SignedInteger for i8;
    impl SignedInteger for i16;
    impl SignedInteger for i32;
    impl SignedInteger for i64;
    impl Float for f32;
    impl Float for f64;
    impl Scalar for bool;
}
