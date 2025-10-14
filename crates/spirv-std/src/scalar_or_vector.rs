use crate::Scalar;
use core::num::NonZeroUsize;

pub(crate) mod sealed {
    /// A marker trait used to prevent other traits from being implemented outside
    /// of `spirv-std`.
    pub trait Sealed {}
}

/// Abstract trait representing either a [`Scalar`] or [`Vector`] type.
///
/// # Safety
/// Your type must also implement [`Scalar`] or [`Vector`], see their safety sections as well.
///
/// [`Vector`]: crate::Vector
pub unsafe trait ScalarOrVector: Copy + Default + Send + Sync + 'static {
    /// Either the scalar component type of the vector or the scalar itself.
    type Scalar: Scalar;

    /// The dimension of the vector, or 1 if it is a scalar
    const N: NonZeroUsize;
}
