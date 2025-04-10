use crate::sealed::Sealed;
use glam::{Vec2, Vec3, Vec3A, Vec4};

#[cfg(target_arch = "spirv")]
macro_rules! deriv_fn {
    ($inst:ident, $param:expr) => {
        unsafe {
            let mut result = Default::default();
            core::arch::asm!(
                "%input = OpLoad typeof*{1} {1}",
                concat!("%result = ", stringify!($inst), " typeof*{1} %input"),
                "OpStore {0} %result",
                in(reg) &mut result,
                in(reg) &$param,
            );
            result
        }
    };
}

/// Types that can be derived by partial derivatives
///
/// # Safety
/// Result Type must be a scalar or vector of floating-point type using the IEEE 754 encoding. The component width must be 32 bits.
pub unsafe trait Derivative: Sealed + Default {
    /// Result is the partial derivative of `Self` with respect to the window x coordinate. Uses local differencing
    /// based on the value of `Self`. Same result as either [`Self::dfdx_fine`] or [`Self::dfdx_coarse`] on `Self`. Selection of which
    /// one is based on external factors.
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn dfdx(self) -> Self {
        deriv_fn!(OpDPdx, self)
    }

    /// Result is the partial derivative of `Self` with respect to the window x coordinate. Uses local differencing
    /// based on the value of `Self` for the current fragment and its immediate neighbor(s).
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn dfdx_fine(self) -> Self {
        deriv_fn!(OpDPdxFine, self)
    }

    /// Result is the partial derivative of `Self` with respect to the window x coordinate. Uses local differencing
    /// based on the value of `Self` for the current fragment’s neighbors, and possibly, but not necessarily, includes
    /// the value of `Self` for the current fragment. That is, over a given area, the implementation can compute x
    /// derivatives in fewer unique locations than would be allowed for [`Self::dfdx_fine`].
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn dfdx_coarse(self) -> Self {
        deriv_fn!(OpDPdxCoarse, self)
    }

    /// Result is the partial derivative of `Self` with respect to the window y coordinate. Uses local differencing
    /// based on the value of `Self`. Same result as either [`Self::dfdy_fine`] or [`Self::dfdy_coarse`] on `Self`. Selection of which
    /// one is based on external factors.
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn dfdy(self) -> Self {
        deriv_fn!(OpDPdy, self)
    }

    /// Result is the partial derivative of `Self` with respect to the window y coordinate. Uses local differencing
    /// based on the value of `Self` for the current fragment and its immediate neighbor(s).
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn dfdy_fine(self) -> Self {
        deriv_fn!(OpDPdyFine, self)
    }

    /// Result is the partial derivative of `Self` with respect to the window y coordinate. Uses local differencing
    /// based on the value of `Self` for the current fragment’s neighbors, and possibly, but not necessarily, includes
    /// the value of `Self` for the current fragment. That is, over a given area, the implementation can compute y
    /// derivatives in fewer unique locations than would be allowed for [`Self::dfdy_fine`].
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn dfdy_coarse(self) -> Self {
        deriv_fn!(OpDPdyCoarse, self)
    }

    /// Result is the same as computing the sum of the absolute values of [`Self::dfdx`] and [`Self::dfdy`] on P.
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn fwidth(self) -> Self {
        deriv_fn!(OpFwidth, self)
    }

    /// Result is the same as computing the sum of the absolute values of [`Self::dfdx_fine`] and [`Self::dfdy_fine`] on P.
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn fwidth_fine(self) -> Self {
        deriv_fn!(OpFwidthFine, self)
    }

    /// Result is the same as computing the sum of the absolute values of [`Self::dfdx_coarse`] and [`Self::dfdy_coarse`] on P.
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn fwidth_coarse(self) -> Self {
        deriv_fn!(OpFwidthCoarse, self)
    }
}

unsafe impl Derivative for f32 {}
unsafe impl Derivative for Vec2 {}
unsafe impl Derivative for Vec3 {}
unsafe impl Derivative for Vec4 {}
unsafe impl Derivative for Vec3A {}
