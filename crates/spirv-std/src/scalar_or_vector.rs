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
/// `VectorOrScalarComposite`.
///
/// To derive it on an enum, the enum must implement `From<N>` and `Into<N>` where `N` is defined by the `#[repr(N)]`
/// attribute on the enum and is an [`Integer`], like `u32`.
/// Note that some [safe subgroup operations] may return an "undefined result", so your `From<N>` must gracefully handle
/// arbitrary bit patterns being passed to it. While panicking is legal, it is discouraged as it may result in
/// unexpected control flow.
/// To implement these conversion traits, we recommend [`FromPrimitive`] and [`IntoPrimitive`] from the [`num_enum`]
/// crate. [`FromPrimitive`] requires that either the enum is exhaustive, or you provide it with a variant to default
/// to, by either implementing [`Default`] or marking a variant with `#[num_enum(default)]`. Note to disable default
/// features on the [`num_enum`] crate, or it won't compile on SPIR-V.
///
/// [`Integer`]: crate::Integer
/// [subgroup operations]: crate::arch::subgroup_shuffle
/// [`FromPrimitive`]: https://docs.rs/num_enum/latest/num_enum/derive.FromPrimitive.html
/// [`IntoPrimitive`]: https://docs.rs/num_enum/latest/num_enum/derive.IntoPrimitive.html
/// [`num_enum`]: https://crates.io/crates/num_enum
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
