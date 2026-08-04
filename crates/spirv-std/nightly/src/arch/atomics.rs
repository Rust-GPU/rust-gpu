use crate::memory::{Scope, Semantics};
use crate::{Float, Integer, Number, SignedInteger, UnsignedInteger};

/// See [`spirv_std::arch::atomic_load`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicLoad")]
#[inline]
pub unsafe fn atomic_load<N: Number, const SCOPE: Scope, const SEMANTICS: Semantics>(ptr: &N) -> N {
    const { SEMANTICS.assert_valid() }
    spirv_std::arch::atomic_load::<N, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr)
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
    spirv_std::arch::atomic_store::<N, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_exchange::<N, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_compare_exchange::<
        I,
        { SCOPE as u32 },
        { EQUAL as u32 },
        { UNEQUAL as u32 },
    >(ptr, value, comparator)
}

/// See [`spirv_std::arch::atomic_i_increment`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIIncrement")]
#[inline]
pub unsafe fn atomic_i_increment<I: Integer, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut I,
) -> I {
    const { SEMANTICS.assert_valid() }
    spirv_std::arch::atomic_i_increment::<I, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr)
}

/// See [`spirv_std::arch::atomic_i_decrement`]
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpAtomicIDecrement")]
#[inline]
pub unsafe fn atomic_i_decrement<I: Integer, const SCOPE: Scope, const SEMANTICS: Semantics>(
    ptr: &mut I,
) -> I {
    const { SEMANTICS.assert_valid() }
    spirv_std::arch::atomic_i_decrement::<I, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr)
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
    spirv_std::arch::atomic_i_add::<I, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_i_sub::<I, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_s_min::<S, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_u_min::<U, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_s_max::<S, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_u_max::<U, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_and::<I, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_or::<I, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_xor::<I, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_f_min::<F, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_f_max::<F, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
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
    spirv_std::arch::atomic_f_add::<F, { SCOPE as u32 }, { SEMANTICS.bits() }>(ptr, value)
}
