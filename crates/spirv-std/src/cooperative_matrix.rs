//! Cooperative matrix types and operations (`SPV_KHR_cooperative_matrix`).
//!
//! Requires the `CooperativeMatrixKHR` capability and `SPV_KHR_cooperative_matrix` extension:
//! ```text
//! -C target-feature=+CooperativeMatrixKHR,+ext:SPV_KHR_cooperative_matrix
//! ```
//!
//! See the [SPV_KHR_cooperative_matrix specification] for full details.
//!
//! [SPV_KHR_cooperative_matrix specification]: https://github.khronos.org/SPIRV-Registry/extensions/KHR/SPV_KHR_cooperative_matrix.html
#[cfg(target_arch = "spirv")]
use core::arch::asm;
use core::marker::PhantomData;
use core::mem::MaybeUninit;

/// Matrix role in a cooperative multiply-accumulate operation (`D = A × B + C`).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum MatrixUse {
    /// Input operand A.
    MatrixA = 0,
    /// Input operand B.
    MatrixB = 1,
    /// Accumulator / result.
    MatrixAccumulator = 2,
}

/// Memory layout for cooperative matrix load/store operations.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum MatrixLayout {
    /// Rows are stored contiguously.
    RowMajor = 0,
    /// Columns are stored contiguously.
    ColumnMajor = 1,
}

/// Matrix role: input operand A in D = A × B + C.
pub const MATRIX_A: u32 = MatrixUse::MatrixA as u32;
/// Matrix role: input operand B in D = A × B + C.
pub const MATRIX_B: u32 = MatrixUse::MatrixB as u32;
/// Matrix role: accumulator / result in D = A × B + C.
pub const MATRIX_ACCUMULATOR: u32 = MatrixUse::MatrixAccumulator as u32;

/// Memory layout: rows are stored contiguously.
pub const ROW_MAJOR: MatrixLayout = MatrixLayout::RowMajor;
/// Memory layout: columns are stored contiguously.
pub const COLUMN_MAJOR: MatrixLayout = MatrixLayout::ColumnMajor;

/// A cooperative matrix distributed across the subgroup.
///
/// Each invocation holds a fragment of the full `ROWS × COLS` matrix.
/// The hardware maps elements to invocations automatically.
///
/// # Type parameters
/// - `T`: element type (`f32`, `f64`, `i32`, `u32`, `i8`, `u8`, etc.)
/// - `USE`: matrix role — one of [`MatrixUse::MatrixA`], [`MatrixUse::MatrixB`], [`MatrixUse::MatrixAccumulator`] cast to `u32`
/// - `ROWS`: number of rows
/// - `COLS`: number of columns
///
/// # Capability
/// Requires `CooperativeMatrixKHR` + `SPV_KHR_cooperative_matrix`.
#[spirv(cooperative_matrix)]
#[derive(Copy, Clone)]
#[repr(C)]
pub struct CooperativeMatrix<T, const USE: u32, const ROWS: u32, const COLS: u32> {
    // HACK: keeps the Rust layout non-ZST so #[spirv(cooperative_matrix)] can
    // special-case it before it gets elided.
    _anti_zst_padding: MaybeUninit<u32>,
    _phantom: PhantomData<T>,
}

impl<T, const USE: u32, const ROWS: u32, const COLS: u32> CooperativeMatrix<T, USE, ROWS, COLS> {
    /// Load a cooperative matrix through a pointer.
    ///
    /// `slice` must point into an array. `layout` specifies whether the matrix
    /// is stored in row-major ([`MatrixLayout::RowMajor`]) or column-major
    /// ([`MatrixLayout::ColumnMajor`]) order. `stride` is the number of elements
    /// between the start of consecutive rows (row-major) or columns (column-major).
    ///
    /// The scope is always `Subgroup`.
    ///
    /// # Safety
    /// - `slice` must point into an array and be valid for all element accesses
    ///   implied by the matrix dimensions, layout, and stride.
    /// - All operands must be dynamically uniform within every instance of the
    ///   subgroup scope.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpCooperativeMatrixLoadKHR")]
    #[inline]
    pub unsafe fn load(slice: &[T], layout: MatrixLayout, stride: u32) -> Self {
        unsafe {
            let mut result = MaybeUninit::<Self>::uninit();
            let layout_u32 = layout as u32;
            let ptr = slice.as_ptr();
            asm!(
                "%u32 = OpTypeInt 32 0",
                "%layout = OpLoad %u32 {layout}",
                "%stride = OpLoad %u32 {stride}",
                // Use typeof* to get the cooperative matrix type from the output pointer.
                "%result = OpCooperativeMatrixLoadKHR typeof* {out} {ptr} %layout %stride",
                "OpStore {out} %result",
                ptr    = in(reg) ptr,
                layout = in(reg) &layout_u32,
                stride = in(reg) &stride,
                out    = in(reg) result.as_mut_ptr(),
            );
            result.assume_init()
        }
    }

    /// Store a cooperative matrix through a pointer.
    ///
    /// `slice` must point into an array. `layout` specifies whether the matrix
    /// is stored in row-major ([`MatrixLayout::RowMajor`]) or column-major
    /// ([`MatrixLayout::ColumnMajor`]) order. `stride` is the number of elements
    /// between the start of consecutive rows (row-major) or columns (column-major).
    ///
    /// The scope is always `Subgroup`.
    ///
    /// # Safety
    /// - `slice` must point into an array and be valid for all element accesses
    ///   implied by the matrix dimensions, layout, and stride.
    /// - All operands must be dynamically uniform within every instance of the
    ///   subgroup scope.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpCooperativeMatrixStoreKHR")]
    #[inline]
    pub unsafe fn store(self, slice: &mut [T], layout: MatrixLayout, stride: u32) {
        unsafe {
            let layout_u32 = layout as u32;
            let ptr = slice.as_mut_ptr();
            asm!(
                "%u32 = OpTypeInt 32 0",
                "%layout = OpLoad %u32 {layout}",
                "%stride = OpLoad %u32 {stride}",
                "%matrix = OpLoad _ {matrix}",
                "OpCooperativeMatrixStoreKHR {ptr} %matrix %layout %stride",
                ptr    = in(reg) ptr,
                matrix = in(reg) &self,
                layout = in(reg) &layout_u32,
                stride = in(reg) &stride,
            );
        }
    }

    /// Returns the number of matrix components this invocation is responsible for.
    ///
    /// The sum across all invocations in the subgroup equals `ROWS * COLS`.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpCooperativeMatrixLengthKHR")]
    #[inline]
    pub fn length(&self) -> u32 {
        unsafe {
            let mut result: u32 = 0;
            asm!(
                "%u32 = OpTypeInt 32 0",
                // typeof* {self_ptr} resolves to the CooperativeMatrix type (pointee of &self).
                "%coop_ty = typeof* {self_ptr}",
                "%result = OpCooperativeMatrixLengthKHR %u32 %coop_ty",
                "OpStore {out} %result",
                self_ptr = in(reg) self,
                out      = in(reg) &mut result,
            );
            result
        }
    }
}

/// Linear-algebraic matrix multiply of `A` by `B` and then component-wise add `C`.
///
/// The order of operations is implementation-dependent. All matrices must have the
/// same scope, which is always subgroup here.
///
/// - `A`: `M × K` matrix with use [`MatrixUse::MatrixA`]
/// - `B`: `K × N` matrix with use [`MatrixUse::MatrixB`]
/// - `C`: `M × N` matrix with use [`MatrixUse::MatrixAccumulator`]
/// - returns `D`: `M × N` accumulator equal to `A × B + C`
///
/// All operands must be dynamically uniform within every instance of the subgroup scope.
///
/// # Capability
/// Requires `CooperativeMatrixKHR` + `SPV_KHR_cooperative_matrix`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpCooperativeMatrixMulAddKHR")]
#[inline]
pub fn mul_add<TA, TB, TC, const M: u32, const N: u32, const K: u32>(
    a: CooperativeMatrix<TA, { MatrixUse::MatrixA as u32 }, M, K>,
    b: CooperativeMatrix<TB, { MatrixUse::MatrixB as u32 }, K, N>,
    c: CooperativeMatrix<TC, { MatrixUse::MatrixAccumulator as u32 }, M, N>,
) -> CooperativeMatrix<TC, { MatrixUse::MatrixAccumulator as u32 }, M, N> {
    unsafe {
        let mut result = MaybeUninit::<
            CooperativeMatrix<TC, { MatrixUse::MatrixAccumulator as u32 }, M, N>,
        >::uninit();
        asm!(
            "%a      = OpLoad _ {a}",
            "%b      = OpLoad _ {b}",
            "%c      = OpLoad _ {c}",
            "%result = OpCooperativeMatrixMulAddKHR _ %a %b %c",
            "OpStore {out} %result",
            a   = in(reg) &a,
            b   = in(reg) &b,
            c   = in(reg) &c,
            out = in(reg) result.as_mut_ptr(),
        );
        result.assume_init()
    }
}
