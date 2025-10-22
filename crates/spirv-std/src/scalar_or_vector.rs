use crate::{Scalar, Vector};
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
pub unsafe trait ScalarOrVector: ScalarOrVectorComposite + Default {
    /// Either the scalar component type of the vector or the scalar itself.
    type Scalar: Scalar;

    /// The dimension of the vector, or 1 if it is a scalar
    const N: NonZeroUsize;
}

/// A `VectorOrScalarComposite` is a type that is either
/// * a [`Scalar`]
/// * a [`Vector`]
/// * an array of `VectorOrScalarComposite`
/// * a struct where all members are `VectorOrScalarComposite`
/// * an enum with a `repr` that is a [`Scalar`]
///
/// By calling [`Self::transform`] you can visit all the individual [`Scalar`] and [`Vector`] values this composite is
/// build out of and transform them into some other value. This is particularly useful for subgroup intrinsics sending
/// data to other threads.
///
/// To derive `#[derive(VectorOrScalarComposite)]` on a struct, all members must also implement
/// `VectorOrScalarComposite`. To derive it on an enum, the enum must have `#[repr(N)]` where `N` is an [`Integer`].
/// Additionally, you must derive `num_enum::FromPrimitive` and `num_enum::ToPrimitive`, which requires the enum to be
/// either exhaustive, implement [`Default`] or a variant of the enum to have the `#[num_enum(default)]` attribute.
///
/// [`Integer`]: crate::Integer
pub trait ScalarOrVectorComposite: Copy + Send + Sync + 'static {
    /// Transform the individual [`Scalar`] and [`Vector`] values of this type to a different value.
    ///
    /// See [`Self`] for more detail.
    fn transform<F: ScalarOrVectorTransform>(self, f: &mut F) -> Self;
}

/// A transform operation for [`ScalarOrVectorComposite::transform`]
pub trait ScalarOrVectorTransform {
    /// transform a [`ScalarOrVector`]
    fn transform<T: ScalarOrVector>(&mut self, value: T) -> T;

    /// transform a [`Scalar`], defaults to [`self.transform`]
    #[inline]
    fn transform_scalar<T: Scalar>(&mut self, value: T) -> T {
        self.transform(value)
    }

    /// transform a [`Vector`], defaults to [`self.transform`]
    #[inline]
    fn transform_vector<V: Vector<S, N>, S: Scalar, const N: usize>(&mut self, value: V) -> V {
        self.transform(value)
    }
}

/// `Default` is unfortunately necessary until rust-gpu improves
impl<T: ScalarOrVectorComposite + Default, const N: usize> ScalarOrVectorComposite for [T; N] {
    #[inline]
    fn transform<F: ScalarOrVectorTransform>(self, f: &mut F) -> Self {
        let mut out = [T::default(); N];
        for i in 0..N {
            out[i] = self[i].transform(f);
        }
        out
    }
}
