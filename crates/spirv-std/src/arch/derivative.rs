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
pub unsafe trait Derivative: Sealed + Default {
    /// Result is the partial derivative of `Self` with respect to the window x coordinate. Uses local differencing
    /// based on the value of `Self`. Same result as either [`ddx_fine`] or [`ddx_coarse`] on `Self`. Selection of which
    /// one is based on external factors.
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn ddx(self) -> Self {
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
    fn ddx_fine(self) -> Self {
        deriv_fn!(OpDPdxFine, self)
    }

    /// Result is the partial derivative of `Self` with respect to the window x coordinate. Uses local differencing
    /// based on the value of `Self` for the current fragment’s neighbors, and possibly, but not necessarily, includes
    /// the value of `Self` for the current fragment. That is, over a given area, the implementation can compute x
    /// derivatives in fewer unique locations than would be allowed for [`ddx_fine`].
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn ddx_coarse(self) -> Self {
        deriv_fn!(OpDPdxCoarse, self)
    }

    /// Result is the partial derivative of `Self` with respect to the window y coordinate. Uses local differencing
    /// based on the value of `Self`. Same result as either [`ddy_fine`] or [`ddy_coarse`] on `Self`. Selection of which
    /// one is based on external factors.
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn ddy(self) -> Self {
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
    fn ddy_fine(self) -> Self {
        deriv_fn!(OpDPdyFine, self)
    }

    /// Result is the partial derivative of `Self` with respect to the window y coordinate. Uses local differencing
    /// based on the value of `Self` for the current fragment’s neighbors, and possibly, but not necessarily, includes
    /// the value of `Self` for the current fragment. That is, over a given area, the implementation can compute y
    /// derivatives in fewer unique locations than would be allowed for [`ddy_fine`].
    ///
    /// An invocation will not execute a dynamic instance of this instruction (X') until all invocations in its
    /// derivative group have executed all dynamic instances that are program-ordered before X'.
    ///
    /// This instruction is only valid in the Fragment Execution Model.
    #[crate::macros::gpu_only]
    #[inline]
    fn ddy_coarse(self) -> Self {
        deriv_fn!(OpDPdyCoarse, self)
    }

    /// Result is the same as computing the sum of the absolute values of [`ddx`] and [`ddy`] on P.
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

    /// Result is the same as computing the sum of the absolute values of [`ddx_fine`] and [`ddy_fine`] on P.
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

    /// Result is the same as computing the sum of the absolute values of [`ddx_coarse`] and [`ddy_coarse`] on P.
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
