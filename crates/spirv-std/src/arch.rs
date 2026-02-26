//! SPIR-V Intrinsics
//!
//! This module is intended as a low level abstraction over SPIR-V instructions.
//! These functions will typically map to a single instruction, and will perform
//! no additional safety checks beyond type-checking.
#[cfg(target_arch = "spirv")]
use crate::Integer;
use crate::{Scalar, SignedInteger, UnsignedInteger, Vector};
#[cfg(target_arch = "spirv")]
use core::arch::asm;

/// Result is true if any component of `vector` is true, otherwise result is
/// false.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAny")]
#[inline]
pub fn any<V: Vector<bool, N>, const N: usize>(vector: V) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%vector = OpLoad _ {vector}",
            "%result = OpAny %bool %vector",
            "OpStore {result} %result",
            vector = in(reg) &vector,
            result = in(reg) &mut result
        }
    }

    result
}

/// Result is true if all components of `vector` is true, otherwise result is
/// false.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAll")]
#[inline]
pub fn all<V: Vector<bool, N>, const N: usize>(vector: V) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%vector = OpLoad _ {vector}",
            "%result = OpAll %bool %vector",
            "OpStore {result} %result",
            vector = in(reg) &vector,
            result = in(reg) &mut result
        }
    }

    result
}

/// Extract a single, dynamically selected, component of a vector.
///
/// # Safety
/// Behavior is undefined if `index`’s value is greater than or equal to the
/// number of components in `vector`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpVectorExtractDynamic")]
#[inline]
pub unsafe fn vector_extract_dynamic<T: Scalar, const N: usize>(
    vector: impl Vector<T, N>,
    index: usize,
) -> T {
    unsafe {
        let mut result = T::default();

        asm! {
            "%vector = OpLoad _ {vector}",
            "%element = OpVectorExtractDynamic _ %vector {index}",
            "OpStore {element} %element",
            vector = in(reg) &vector,
            index = in(reg) index,
            element = in(reg) &mut result
        }

        result
    }
}

/// Make a copy of a vector, with a single, variably selected,
/// component modified.
///
/// # Safety
/// Behavior is undefined if `index`’s value is greater than or equal to the
/// number of components in `vector`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpVectorInsertDynamic")]
#[inline]
pub unsafe fn vector_insert_dynamic<T: Scalar, V: Vector<T, N>, const N: usize>(
    vector: V,
    index: usize,
    element: T,
) -> V {
    unsafe {
        let mut result = V::default();

        asm! {
            "%vector = OpLoad _ {vector}",
            "%element = OpLoad _ {element}",
            "%new_vector = OpVectorInsertDynamic _ %vector %element {index}",
            "OpStore {result} %new_vector",
            vector = in(reg) &vector,
            index = in(reg) index,
            element = in(reg) &element,
            result = in(reg) &mut result,
        }

        result
    }
}

#[cfg(target_arch = "spirv")]
unsafe fn call_glsl_op_with_ints<T: Integer, const OP: u32>(a: T, b: T) -> T {
    unsafe {
        let mut result = T::default();
        asm!(
            "%glsl = OpExtInstImport \"GLSL.std.450\"",
            "%a = OpLoad _ {a}",
            "%b = OpLoad _ {b}",
            "%result = OpExtInst typeof*{result} %glsl {op} %a %b",
            "OpStore {result} %result",
            a = in(reg) &a,
            b = in(reg) &b,
            result = in(reg) &mut result,
            op = const OP
        );
        result
    }
}

/// Compute the minimum of two unsigned integers via a GLSL extended instruction.
#[spirv_std_macros::gpu_only]
pub fn unsigned_min<T: UnsignedInteger>(a: T, b: T) -> T {
    unsafe { call_glsl_op_with_ints::<_, 38>(a, b) }
}

/// Compute the maximum of two unsigned integers via a GLSL extended instruction.
#[spirv_std_macros::gpu_only]
pub fn unsigned_max<T: UnsignedInteger>(a: T, b: T) -> T {
    unsafe { call_glsl_op_with_ints::<_, 41>(a, b) }
}

/// Compute the minimum of two signed integers via a GLSL extended instruction.
#[spirv_std_macros::gpu_only]
pub fn signed_min<T: SignedInteger>(a: T, b: T) -> T {
    unsafe { call_glsl_op_with_ints::<_, 39>(a, b) }
}

/// Compute the maximum of two signed integers via a GLSL extended instruction.
#[spirv_std_macros::gpu_only]
pub fn signed_max<T: SignedInteger>(a: T, b: T) -> T {
    unsafe { call_glsl_op_with_ints::<_, 42>(a, b) }
}

/// Index into an array without bounds checking.
///
/// The main purpose of this trait is to work around the fact that the regular `get_unchecked*`
/// methods do not work in in SPIR-V.
pub trait IndexUnchecked<T> {
    /// Returns a reference to the element at `index`. The equivalent of `get_unchecked`.
    ///
    /// # Safety
    /// Behavior is undefined if the `index` value is greater than or equal to the length of the array.
    unsafe fn index_unchecked(&self, index: usize) -> &T;
    /// Returns a mutable reference to the element at `index`. The equivalent of `get_unchecked_mut`.
    ///
    /// # Safety
    /// Behavior is undefined if the `index` value is greater than or equal to the length of the array.
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T;
}

impl<T> IndexUnchecked<T> for [T] {
    #[cfg(target_arch = "spirv")]
    unsafe fn index_unchecked(&self, index: usize) -> &T {
        unsafe {
            // FIXME(eddyb) `let mut result = T::default()` uses (for `asm!`), with this.
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%slice_ptr = OpLoad _ {slice_ptr_ptr}",
                "%data_ptr = OpCompositeExtract _ %slice_ptr 0",
                "%result = OpAccessChain _ %data_ptr {index}",
                "OpStore {result_slot} %result",
                slice_ptr_ptr = in(reg) &self,
                index = in(reg) index,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }

    #[cfg(not(target_arch = "spirv"))]
    unsafe fn index_unchecked(&self, index: usize) -> &T {
        unsafe { self.get_unchecked(index) }
    }

    #[cfg(target_arch = "spirv")]
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe {
            // FIXME(eddyb) `let mut result = T::default()` uses (for `asm!`), with this.
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%slice_ptr = OpLoad _ {slice_ptr_ptr}",
                "%data_ptr = OpCompositeExtract _ %slice_ptr 0",
                "%result = OpAccessChain _ %data_ptr {index}",
                "OpStore {result_slot} %result",
                slice_ptr_ptr = in(reg) &self,
                index = in(reg) index,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }

    #[cfg(not(target_arch = "spirv"))]
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe { self.get_unchecked_mut(index) }
    }
}

impl<T, const N: usize> IndexUnchecked<T> for [T; N] {
    #[cfg(target_arch = "spirv")]
    unsafe fn index_unchecked(&self, index: usize) -> &T {
        unsafe {
            // FIXME(eddyb) `let mut result = T::default()` uses (for `asm!`), with this.
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%result = OpAccessChain _ {array_ptr} {index}",
                "OpStore {result_slot} %result",
                array_ptr = in(reg) self,
                index = in(reg) index,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }

    #[cfg(not(target_arch = "spirv"))]
    unsafe fn index_unchecked(&self, index: usize) -> &T {
        unsafe { self.get_unchecked(index) }
    }

    #[cfg(target_arch = "spirv")]
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe {
            // FIXME(eddyb) `let mut result = T::default()` uses (for `asm!`), with this.
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%result = OpAccessChain _ {array_ptr} {index}",
                "OpStore {result_slot} %result",
                array_ptr = in(reg) self,
                index = in(reg) index,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }

    #[cfg(not(target_arch = "spirv"))]
    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe { self.get_unchecked_mut(index) }
    }
}
