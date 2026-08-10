use crate::memory::{Scope, Semantics};
use crate::{Float, Integer, Number, SignedInteger, UnsignedInteger};
#[cfg(target_arch = "spirv")]
use core::arch::asm;

/// See [`spirv_std::arch::atomic_load`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicLoad")]
#[inline]
pub unsafe fn atomic_load<N: Number, const SCOPE: Scope, const SEMANTICS: Semantics>(ptr: &N) -> N {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut result = N::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%result = OpAtomicLoad _ {ptr} %scope %semantics",
            "OpStore {result} %result",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            result = in(reg) &mut result
        }
        result
    }
}

/// See [`spirv_std::arch::atomic_store`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicStore")]
#[inline]
pub unsafe fn atomic_store<N: Number, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut N,
    value: N,
) {
    const { SEMANTICS.assert_valid() }
    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "OpAtomicStore {ptr} %scope %semantics %value",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            value = in(reg) &value
        }
    }
}

/// See [`spirv_std::arch::atomic_exchange`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicExchange")]
#[inline]
pub unsafe fn atomic_exchange<N: Number, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut N,
    value: N,
) -> N {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = N::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicExchange _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_compare_exchange`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicCompareExchange")]
#[inline]
pub unsafe fn atomic_compare_exchange<
    I: Integer,
    const SCOPE: Scope,
    const EQUAL: Semantics,
    const UNEQUAL: Semantics,
>(
    ptr: &mut I,
    value: I,
    comparator: I,
) -> I {
    const {
        EQUAL.assert_valid();
        UNEQUAL.assert_valid();
    }
    unsafe {
        let mut old = I::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%equal = OpConstant %u32 {equal}",
            "%unequal = OpConstant %u32 {unequal}",
            "%value = OpLoad _ {value}",
            "%comparator = OpLoad _ {comparator}",
            "%old = OpAtomicCompareExchange _ {ptr} %scope %equal %unequal %value %comparator",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            equal = const EQUAL.bits(),
            unequal = const UNEQUAL.bits(),
            ptr = in(reg) ptr,
            value = in(reg) &value,
            comparator = in(reg) &comparator,
            old = in(reg) &mut old,
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_i_increment`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIIncrement")]
#[inline]
pub unsafe fn atomic_i_increment<I: Integer, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut I,
) -> I {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = I::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%old = OpAtomicIIncrement _ {ptr} %scope %semantics",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_i_decrement`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIDecrement")]
#[inline]
pub unsafe fn atomic_i_decrement<I: Integer, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut I,
) -> I {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = I::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%old = OpAtomicIDecrement _ {ptr} %scope %semantics",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_i_add`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIAdd")]
#[inline]
pub unsafe fn atomic_i_add<I: Integer, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut I,
    value: I,
) -> I {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = I::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicIAdd _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_i_sub`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicISub")]
#[inline]
pub unsafe fn atomic_i_sub<I: Integer, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut I,
    value: I,
) -> I {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = I::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicISub _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_s_min`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicSMin")]
#[inline]
pub unsafe fn atomic_s_min<S: SignedInteger, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut S,
    value: S,
) -> S {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = S::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicSMin _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_u_min`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicUMin")]
#[inline]
pub unsafe fn atomic_u_min<U: UnsignedInteger, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut U,
    value: U,
) -> U {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = U::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicUMin _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_s_max`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicSMax")]
#[inline]
pub unsafe fn atomic_s_max<S: SignedInteger, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut S,
    value: S,
) -> S {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = S::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicSMax _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_u_max`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicUMax")]
#[inline]
pub unsafe fn atomic_u_max<U: UnsignedInteger, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut U,
    value: U,
) -> U {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = U::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicUMax _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_and`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicAnd")]
#[inline]
pub unsafe fn atomic_and<I: Integer, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut I,
    value: I,
) -> I {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = I::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicAnd _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_or`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicOr")]
#[inline]
pub unsafe fn atomic_or<I: Integer, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut I,
    value: I,
) -> I {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = I::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicOr _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_xor`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicXor")]
#[inline]
pub unsafe fn atomic_xor<I: Integer, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut I,
    value: I,
) -> I {
    const { SEMANTICS.assert_valid() }

    unsafe {
        let mut old = I::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicXor _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_f_min`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicFMinEXT")]
#[inline]
pub unsafe fn atomic_f_min<F: Float, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut F,
    value: F,
) -> F {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = F::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicFMinEXT _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_f_max`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicFMaxEXT")]
#[inline]
pub unsafe fn atomic_f_max<F: Float, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut F,
    value: F,
) -> F {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = F::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicFMaxEXT _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}

/// See [`spirv_std::arch::atomic_f_add`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicFAddEXT")]
#[inline]
pub unsafe fn atomic_f_add<F: Float, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut F,
    value: F,
) -> F {
    const { SEMANTICS.assert_valid() }
    unsafe {
        let mut old = F::default();
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%scope = OpConstant %u32 {scope}",
            "%semantics = OpConstant %u32 {semantics}",
            "%value = OpLoad _ {value}",
            "%old = OpAtomicFAddEXT _ {ptr} %scope %semantics %value",
            "OpStore {old} %old",
            scope = const SCOPE as u32,
            semantics = const SEMANTICS.bits(),
            ptr = in(reg) ptr,
            old = in(reg) &mut old,
            value = in(reg) &value
        }
        old
    }
}
