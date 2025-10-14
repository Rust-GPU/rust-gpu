use crate::ScalarOrVector;
#[cfg(target_arch = "spirv")]
use crate::arch::barrier;
#[cfg(target_arch = "spirv")]
use crate::memory::{Scope, Semantics};
use crate::{Float, Integer, SignedInteger, UnsignedInteger};
#[cfg(target_arch = "spirv")]
use core::arch::asm;

#[cfg(target_arch = "spirv")]
const SUBGROUP: u32 = Scope::Subgroup as u32;

/// `SubgroupMask` is a [`glam::UVec4`] representing a bitmask of all invocations within a subgroup.
/// Mostly used in group ballot operations.
pub type SubgroupMask = glam::UVec4;

/// Defines the class of group operation.
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq)]
pub enum GroupOperation {
    /// A reduction operation for all values of a specific value X specified by invocations within a workgroup.
    Reduce = 0,
    /// A binary operation with an identity I and n (where n is the size of the workgroup)
    /// elements[a0, a1, … an-1] resulting in [a0, (a0 op a1), …(a0 op a1 op … op an-1)]
    InclusiveScan = 1,
    /// A binary operation with an identity I and n (where n is the size of the workgroup)
    /// elements[a0, a1, … an-1] resulting in [I, a0, (a0 op a1), … (a0 op a1 op … op an-2)].
    ExclusiveScan = 2,
    /// The [`GroupOperation`] `ClusteredReduce`.
    ///
    /// All instructions with a [`GroupOperation`] require an additional `ClusterSize` parameter when [`GroupOperation`] is
    /// `ClusteredReduce`. To map this requirement into rust, all function have a base version accepting [`GroupOperation`]
    /// as a const generic, and a `_clustered` variant that is fixed to `ClusteredReduce` and takes the additional
    /// `ClusterSize` parameter as a const generic. To not accidentally use a `ClusteredReduce` in the base variant of the
    /// function, it was removed from the [`GroupOperation`] enum and instead resides individually.
    ClusteredReduce = 3,
    /// Reserved.
    ///
    /// Requires Capability `GroupNonUniformPartitionedNV`.
    PartitionedReduceNV = 6,
    /// Reserved.
    ///
    /// Requires Capability `GroupNonUniformPartitionedNV`.
    PartitionedInclusiveScanNV = 7,
    /// Reserved.
    ///
    /// Requires Capability `GroupNonUniformPartitionedNV`.
    PartitionedExclusiveScanNV = 8,
}

/// The function `subgroupBarrier()` enforces that all active invocations within a
/// subgroup must execute this function before any are allowed to continue their
/// execution, and the results of any memory stores performed using coherent
/// variables performed prior to the call will be visible to any future
/// coherent access to the same memory performed by any other shader invocation
/// within the same subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupBarrier")]
#[inline]
pub fn subgroup_barrier() {
    barrier::control_barrier::<
        SUBGROUP,
        SUBGROUP,
        {
            Semantics::ACQUIRE_RELEASE.bits()
                | Semantics::UNIFORM_MEMORY.bits()
                | Semantics::WORKGROUP_MEMORY.bits()
                | Semantics::IMAGE_MEMORY.bits()
        },
    >();
}

/// The function `subgroupMemoryBarrier()` enforces the ordering of all memory
/// transactions issued within a single shader invocation, as viewed by other
/// invocations in the same subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupMemoryBarrier")]
#[inline]
pub fn subgroup_memory_barrier() {
    barrier::memory_barrier::<
        SUBGROUP,
        {
            Semantics::ACQUIRE_RELEASE.bits()
                | Semantics::UNIFORM_MEMORY.bits()
                | Semantics::WORKGROUP_MEMORY.bits()
                | Semantics::IMAGE_MEMORY.bits()
        },
    >();
}

/// The function `subgroupMemoryBarrierBuffer()` enforces the ordering of all
/// memory transactions to buffer variables issued within a single shader
/// invocation, as viewed by other invocations in the same subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupMemoryBarrierBuffer")]
#[inline]
pub fn subgroup_memory_barrier_buffer() {
    barrier::memory_barrier::<
        SUBGROUP,
        { Semantics::ACQUIRE_RELEASE.bits() | Semantics::UNIFORM_MEMORY.bits() },
    >();
}

/// The function `subgroupMemoryBarrierShared()` enforces the ordering of all
/// memory transactions to shared variables issued within a single shader
/// invocation, as viewed by other invocations in the same subgroup.
///
/// Only available in compute shaders.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupMemoryBarrierShared")]
#[inline]
pub fn subgroup_memory_barrier_shared() {
    barrier::memory_barrier::<
        SUBGROUP,
        { Semantics::ACQUIRE_RELEASE.bits() | Semantics::WORKGROUP_MEMORY.bits() },
    >();
}

/// The function `subgroupMemoryBarrierImage()` enforces the ordering of all
/// memory transactions to images issued within a single shader invocation, as
/// viewed by other invocations in the same subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupMemoryBarrierImage")]
#[inline]
pub fn subgroup_memory_barrier_image() {
    barrier::memory_barrier::<
        SUBGROUP,
        { Semantics::ACQUIRE_RELEASE.bits() | Semantics::IMAGE_MEMORY.bits() },
    >();
}

/// Result is true only in the active invocation with the lowest id in the group, otherwise result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformElect")]
#[inline]
pub fn subgroup_elect() -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%result = OpGroupNonUniformElect %bool %subgroup",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a `predicate` for all active invocations in the group, resulting in true if `predicate` evaluates to true for all active invocations in the group, otherwise the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// `predicate` must be a Boolean type.
///
/// Requires Capability `GroupNonUniformVote`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformAll")]
#[inline]
pub fn subgroup_all(predicate: bool) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%predicate = OpLoad _ {predicate}",
            "%result = OpGroupNonUniformAll %bool %subgroup %predicate",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            predicate = in(reg) &predicate,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a `predicate` for all active invocations in the group, resulting in true if `predicate` evaluates to true for any active invocation in the group, otherwise the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// `predicate` must be a Boolean type.
///
/// Requires Capability `GroupNonUniformVote`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformAny")]
#[inline]
pub fn subgroup_any(predicate: bool) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%predicate = OpLoad _ {predicate}",
            "%result = OpGroupNonUniformAny %bool %subgroup %predicate",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            predicate = in(reg) &predicate,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a `value` for all active invocations in the group. The result is true if `value` is equal for all active invocations in the group. Otherwise, the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// `value` must be a scalar or vector of floating-point type, integer type, or Boolean type. The compare operation is based on this type, and if it is a floating-point type, an ordered-and-equal compare is used.
///
/// Requires Capability `GroupNonUniformVote`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformAllEqual")]
#[inline]
pub fn subgroup_all_equal<T: ScalarOrVector>(value: T) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformAllEqual %bool %subgroup %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the `value` of the invocation identified by the id `id` to all active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of `value` must be the same as Result Type.
///
/// `id` must be a scalar of integer type, whose Signedness operand is 0.
///
/// Before version 1.5, `id` must come from a constant instruction. Starting with version 1.5, this restriction is lifted. However, behavior is undefined when `id` is not dynamically uniform.
///
/// The resulting value is undefined if `id` is an inactive invocation, or is greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformBallot`.
///
/// # Safety
/// * `id` must not be dynamically uniform
/// * before 1.5: `id` must be constant
/// * Result is undefined if `id` is an inactive invocation or out of bounds
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBroadcast")]
#[inline]
pub unsafe fn subgroup_broadcast<T: ScalarOrVector>(value: T, id: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%id = OpLoad _ {id}",
            "%result = OpGroupNonUniformBroadcast _ %subgroup %value %id",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            id = in(reg) &id,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the `value` of the invocation from the active invocation with the lowest id in the group to all active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of `value` must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBroadcastFirst")]
#[inline]
pub fn subgroup_broadcast_first<T: ScalarOrVector>(value: T) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformBroadcastFirst _ %subgroup %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is a bitfield value combining the `predicate` value from all invocations in the group that execute the same dynamic instance of this instruction. The bit is set to one if the corresponding invocation is active and the `predicate` for that invocation evaluated to true; otherwise, it is set to zero.
///
/// Result Type must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// Result is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Execution is a Scope that identifies the group of invocations affected by this command.
///
/// `predicate` must be a Boolean type.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBallot")]
#[inline]
pub fn subgroup_ballot(predicate: bool) -> SubgroupMask {
    let mut result = SubgroupMask::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%predicate = OpLoad _ {predicate}",
            "%result = OpGroupNonUniformBallot typeof*{result} %subgroup %predicate",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            predicate = in(reg) &predicate,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a `value` for all active invocations in the group, resulting in true if the bit in `value` for the corresponding invocation is set to one, otherwise the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// `value` must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// Behavior is undefined unless `value` is the same for all invocations that execute the same dynamic instance of this instruction.
///
/// `value` is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Requires Capability `GroupNonUniformBallot`.
///
/// # Safety
/// * `value` must be the same for all dynamic instances of this instruction
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformInverseBallot")]
#[inline]
pub unsafe fn subgroup_inverse_ballot(value: SubgroupMask) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformInverseBallot %bool %subgroup %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a value for all active invocations in the group, resulting in true if the bit in `value` that corresponds to `index` is set to one, otherwise the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// `value` must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// `value` is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// `index` must be a scalar of integer type, whose Signedness operand is 0.
///
/// The resulting value is undefined if `index` is greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformBallot`.
///
/// # Safety
/// * This function is safe
/// * Result is undefined if `id` is out of bounds
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBallotBitExtract")]
#[inline]
pub fn subgroup_ballot_bit_extract(value: SubgroupMask, index: u32) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%index = OpLoad _ {index}",
            "%result = OpGroupNonUniformBallotBitExtract %bool %subgroup %value %index",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            index = in(reg) &index,
            result = in(reg) &mut result,
        }
    }

    result
}

macro_rules! macro_subgroup_ballot_bit_count {
    ($name:ident, $group_op:expr) => {
        /// Result is the number of bits that are set to 1 in `value`, considering only the bits in `value` required to represent all bits of the group's invocations.
        ///
        /// Result Type must be a scalar of integer type, whose Signedness operand is 0.
        ///
        /// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
        ///
        /// The identity I for Operation is 0.
        ///
        /// `value` must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
        ///
        /// `value` is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
        ///
        /// Requires Capability `GroupNonUniformBallot`.
        #[spirv_std_macros::gpu_only]
        #[doc(alias = "OpGroupNonUniformBallotBitCount")]
        #[inline]
        pub fn $name(value: SubgroupMask) -> u32 {
            let mut result = 0;

            unsafe {
                asm! {
                    "%u32 = OpTypeInt 32 0",
                    "%subgroup = OpConstant %u32 {subgroup}",
                    "%value = OpLoad _ {value}",
                    "%result = OpGroupNonUniformBallotBitCount %u32 %subgroup {groupop} %value",
                    "OpStore {result} %result",
                    subgroup = const SUBGROUP,
                    groupop = const ($group_op as u32),
                    value = in(reg) &value,
                    result = in(reg) &mut result,
                }
            }

            result
        }
    };
}

macro_subgroup_ballot_bit_count!(subgroup_ballot_bit_count, GroupOperation::Reduce);
macro_subgroup_ballot_bit_count!(
    subgroup_ballot_inclusive_bit_count,
    GroupOperation::InclusiveScan
);
macro_subgroup_ballot_bit_count!(
    subgroup_ballot_exclusive_bit_count,
    GroupOperation::ExclusiveScan
);

/// Find the least significant bit set to 1 in `value`, considering only the bits in `value` required to represent all bits of the group's invocations. If none of the considered bits is set to 1, the resulting value is undefined.
///
/// Result Type must be a scalar of integer type, whose Signedness operand is 0.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// `value` must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// `value` is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Requires Capability `GroupNonUniformBallot`.
///
/// # Safety
/// * This function is safe
/// * Result is undefined if `id` is an inactive invocation or out of bounds
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBallotFindLSB")]
#[inline]
pub fn subgroup_ballot_find_lsb(value: SubgroupMask) -> u32 {
    let mut result = 0;

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformBallotFindLSB %u32 %subgroup %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Find the most significant bit set to 1 in `value`, considering only the bits in `value` required to represent all bits of the group's invocations. If none of the considered bits is set to 1, the resulting value is undefined.
///
/// Result Type must be a scalar of integer type, whose Signedness operand is 0.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// `value` must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// `value` is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBallotFindMSB")]
#[inline]
pub fn subgroup_ballot_find_msb(value: SubgroupMask) -> u32 {
    let mut result = 0;

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformBallotFindMSB %u32 %subgroup %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the `value` of the invocation identified by the id `id`.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command.
///
/// The type of `value` must be the same as Result Type.
///
/// `id` must be a scalar of integer type, whose Signedness operand is 0.
///
/// The resulting value is undefined if `id` is an inactive invocation, or is greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformShuffle`.
///
/// # Safety
/// * This function is safe
/// * Result is undefined if `id` is an inactive invocation or out of bounds
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformShuffle")]
#[inline]
pub fn subgroup_shuffle<T: ScalarOrVector>(value: T, id: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%id = OpLoad _ {id}",
            "%result = OpGroupNonUniformShuffle _ %subgroup %value %id",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            id = in(reg) &id,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the `value` of the invocation identified by the current invocation’s id within the group xor’ed with Mask.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of `value` must be the same as Result Type.
///
/// Mask must be a scalar of integer type, whose Signedness operand is 0.
///
/// The resulting value is undefined if current invocation’s id within the group xor’ed with Mask is an inactive invocation, or is greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformShuffle`.
///
/// # Safety
/// * This function is safe
/// * Result is undefined if current invocation’s id within the group xor’ed with `mask` is an inactive invocation or out of bounds
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformShuffleXor")]
#[inline]
pub fn subgroup_shuffle_xor<T: ScalarOrVector>(value: T, mask: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%mask = OpLoad _ {mask}",
            "%result = OpGroupNonUniformShuffleXor _ %subgroup %value %mask",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            mask = in(reg) &mask,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the `value` of the invocation identified by the current invocation’s id within the group - Delta.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of `value` must be the same as Result Type.
///
/// Delta must be a scalar of integer type, whose Signedness operand is 0.
///
/// Delta is treated as unsigned and the resulting value is undefined if Delta is greater than the current invocation’s id within the group or if the selected lane is inactive.
///
/// Requires Capability `GroupNonUniformShuffleRelative`.
///
/// # Safety
/// * This function is safe
/// * Result is undefined if `delta` is greater than the current invocation’s id within the group or if the selected lane is inactive
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformShuffleUp")]
#[inline]
pub fn subgroup_shuffle_up<T: ScalarOrVector>(value: T, delta: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%delta = OpLoad _ {delta}",
            "%result = OpGroupNonUniformShuffleUp _ %subgroup %value %delta",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            delta = in(reg) &delta,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the `value` of the invocation identified by the current invocation’s id within the group + Delta.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of `value` must be the same as Result Type.
///
/// Delta must be a scalar of integer type, whose Signedness operand is 0.
///
/// Delta is treated as unsigned and the resulting value is undefined if Delta is greater than or equal to the size of the group, or if the current invocation’s id within the group + Delta is either an inactive invocation or greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformShuffleRelative`.
///
/// # Safety
/// * This function is safe
/// * Result is undefined if `delta` is greater than or equal to the size of the group, or if the current invocation’s id within the group + `delta` is either an inactive invocation or greater than or equal to the size of the group.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformShuffleDown")]
#[inline]
pub fn subgroup_shuffle_down<T: ScalarOrVector>(value: T, delta: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%delta = OpLoad _ {delta}",
            "%result = OpGroupNonUniformShuffleDown _ %subgroup %value %delta",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            delta = in(reg) &delta,
            result = in(reg) &mut result,
        }
    }

    result
}

macro_rules! macro_subgroup_op {
    ($scalar:ty, $asm_op:literal, $($name:ident, $group_op:expr),+; $docs:literal) => { $(
        #[doc = $docs]
        #[spirv_std_macros::gpu_only]
        #[doc(alias = $asm_op)]
        #[inline]
        pub fn $name<I: ScalarOrVector<Scalar = $scalar>>(
            value: I,
        ) -> I {
            let mut result = I::default();
            unsafe {
                asm! {
                    "%u32 = OpTypeInt 32 0",
                    "%subgroup = OpConstant %u32 {subgroup}",
                    "%value = OpLoad _ {value}",
                    concat!("%result = ", $asm_op, " _ %subgroup {groupop} %value"),
                    "OpStore {result} %result",
                    subgroup = const SUBGROUP,
                    groupop = const ($group_op as u32),
                    value = in(reg) &value,
                    result = in(reg) &mut result,
                }
            }
            result
        }
    )+ };
}

macro_rules! macro_subgroup_op_clustered {
    ($scalar:ty, $asm_op:literal, $name:ident; $docs:literal) => {
        #[doc = $docs]
        #[spirv_std_macros::gpu_only]
        #[doc(alias = $asm_op)]
        #[inline]
        pub unsafe fn $name<const CLUSTER_SIZE: u32, I: ScalarOrVector<Scalar = $scalar>>(
            value: I,
        ) -> I {
            const {
                assert!(CLUSTER_SIZE >= 1, "`ClusterSize` must be at least 1");
                assert!(
                    CLUSTER_SIZE.is_power_of_two(),
                    "`ClusterSize` must be a power of 2"
                );
                // Cannot be verified with static assertions:
                // `ClusterSize` must not be greater than the size of the group
            }

            let mut result = I::default();

            unsafe {
                asm! {
                    "%u32 = OpTypeInt 32 0",
                    "%subgroup = OpConstant %u32 {subgroup}",
                    "%value = OpLoad _ {value}",
                    "%clustersize = OpConstant %u32 {clustersize}",
                    concat!("%result = ", $asm_op, " _ %subgroup {groupop} %value %clustersize"),
                    "OpStore {result} %result",
                    subgroup = const SUBGROUP,
                    groupop = const (GroupOperation::ClusteredReduce as u32),
                    clustersize = const CLUSTER_SIZE,
                    value = in(reg) &value,
                    result = in(reg) &mut result,
                }
            }

            result
        }
    };
}

// add
macro_subgroup_op!(impl Integer, "OpGroupNonUniformIAdd", subgroup_i_add, GroupOperation::Reduce, subgroup_inclusive_i_add, GroupOperation::InclusiveScan, subgroup_exclusive_i_add, GroupOperation::ExclusiveScan; r"
An integer add group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl Integer, "OpGroupNonUniformIAdd", subgroup_clustered_i_add; r"
An integer add group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");
macro_subgroup_op!(impl Float, "OpGroupNonUniformFAdd", subgroup_f_add, GroupOperation::Reduce, subgroup_inclusive_f_add, GroupOperation::InclusiveScan, subgroup_exclusive_f_add, GroupOperation::ExclusiveScan; r"
A floating point add group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of floating-point type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0.

The type of `value` must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl Float, "OpGroupNonUniformFAdd", subgroup_clustered_f_add; r"
A floating point add group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of floating-point type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

// mul
macro_subgroup_op!(impl Integer, "OpGroupNonUniformIMul", subgroup_i_mul, GroupOperation::Reduce, subgroup_inclusive_i_mul, GroupOperation::InclusiveScan, subgroup_exclusive_i_mul, GroupOperation::ExclusiveScan; r"
An integer multiply group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 1.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl Integer, "OpGroupNonUniformIMul", subgroup_clustered_i_mul; r"
An integer multiply group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 1. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");
macro_subgroup_op!(impl Float, "OpGroupNonUniformFMul", subgroup_f_mul, GroupOperation::Reduce, subgroup_inclusive_f_mul, GroupOperation::InclusiveScan, subgroup_exclusive_f_mul, GroupOperation::ExclusiveScan; r"
A floating point multiply group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of floating-point type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 1.

The type of `value` must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl Float, "OpGroupNonUniformFMul", subgroup_clustered_f_mul; r"
A floating point multiply group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of floating-point type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 1. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

// min
macro_subgroup_op!(impl SignedInteger, "OpGroupNonUniformSMin", subgroup_s_min, GroupOperation::Reduce, subgroup_inclusive_s_min, GroupOperation::InclusiveScan, subgroup_exclusive_s_min, GroupOperation::ExclusiveScan; r"
A signed integer minimum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is `INT_MAX`.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl SignedInteger, "OpGroupNonUniformSMin", subgroup_clustered_s_min; r"
A signed integer minimum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is `INT_MAX`. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");
macro_subgroup_op!(impl UnsignedInteger, "OpGroupNonUniformUMin", subgroup_u_min, GroupOperation::Reduce, subgroup_inclusive_u_min, GroupOperation::InclusiveScan, subgroup_exclusive_u_min, GroupOperation::ExclusiveScan; r"
An unsigned integer minimum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type, whose Signedness operand is 0.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is `UINT_MAX`.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl UnsignedInteger, "OpGroupNonUniformUMin", subgroup_clustered_u_min; r"
An unsigned integer minimum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type, whose Signedness operand is 0.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is `UINT_MAX`. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");
macro_subgroup_op!(impl Float, "OpGroupNonUniformFMin", subgroup_f_min, GroupOperation::Reduce, subgroup_inclusive_f_min, GroupOperation::InclusiveScan, subgroup_exclusive_f_min, GroupOperation::ExclusiveScan; r"
A floating point minimum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of floating-point type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is +INF.

The type of `value` must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined. From the set of Value(s) provided by active invocations within a subgroup, if for any two Values one of them is a NaN, the other is chosen. If all Value(s) that are used by the current invocation are NaN, then the result is an undefined value.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl Float, "OpGroupNonUniformFMin", subgroup_clustered_f_min; r"
A floating point minimum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of floating-point type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is +INF. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined. From the set of Value(s) provided by active invocations within a subgroup, if for any two Values one of them is a NaN, the other is chosen. If all Value(s) that are used by the current invocation are NaN, then the result is an undefined value.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

// max
macro_subgroup_op!(impl SignedInteger, "OpGroupNonUniformSMax", subgroup_s_max, GroupOperation::Reduce, subgroup_inclusive_s_max, GroupOperation::InclusiveScan, subgroup_exclusive_s_max, GroupOperation::ExclusiveScan; r"
A signed integer maximum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is `INT_MIN`.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl SignedInteger, "OpGroupNonUniformSMax", subgroup_clustered_s_max; r"
A signed integer maximum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is `INT_MIN`. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");
macro_subgroup_op!(impl UnsignedInteger, "OpGroupNonUniformUMax", subgroup_u_max, GroupOperation::Reduce, subgroup_inclusive_u_max, GroupOperation::InclusiveScan, subgroup_exclusive_u_max, GroupOperation::ExclusiveScan; r"
An unsigned integer maximum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type, whose Signedness operand is 0.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl UnsignedInteger, "OpGroupNonUniformUMax", subgroup_clustered_u_max; r"
An unsigned integer maximum group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type, whose Signedness operand is 0.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");
macro_subgroup_op!(impl Float, "OpGroupNonUniformFMax", subgroup_f_max, GroupOperation::Reduce, subgroup_inclusive_f_max, GroupOperation::InclusiveScan, subgroup_exclusive_f_max, GroupOperation::ExclusiveScan; r"
A floating point maximum group operation of all `value` operands contributed by active invocations in by group.

Result Type must be a scalar or vector of floating-point type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is -INF.

The type of `value` must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined. From the set of Value(s) provided by active invocations within a subgroup, if for any two Values one of them is a NaN, the other is chosen. If all Value(s) that are used by the current invocation are NaN, then the result is an undefined value.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl Float, "OpGroupNonUniformFMax", subgroup_clustered_f_max; r"
A floating point maximum group operation of all `value` operands contributed by active invocations in by group.

Result Type must be a scalar or vector of floating-point type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is -INF.

The type of `value` must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined. From the set of Value(s) provided by active invocations within a subgroup, if for any two Values one of them is a NaN, the other is chosen. If all Value(s) that are used by the current invocation are NaN, then the result is an undefined value.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

// and
macro_subgroup_op!(impl Integer, "OpGroupNonUniformBitwiseAnd", subgroup_and, GroupOperation::Reduce, subgroup_inclusive_and, GroupOperation::InclusiveScan, subgroup_exclusive_and, GroupOperation::ExclusiveScan; r"
A bitwise and group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is ~0.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl Integer, "OpGroupNonUniformBitwiseAnd", subgroup_clustered_and; r"
A bitwise and group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is ~0. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

// or
macro_subgroup_op!(impl Integer, "OpGroupNonUniformBitwiseOr", subgroup_or, GroupOperation::Reduce, subgroup_inclusive_or, GroupOperation::InclusiveScan, subgroup_exclusive_or, GroupOperation::ExclusiveScan; r"
A bitwise or group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl Integer, "OpGroupNonUniformBitwiseOr", subgroup_clustered_or; r"
A bitwise or group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

// xor
macro_subgroup_op!(impl Integer, "OpGroupNonUniformBitwiseXor", subgroup_xor, GroupOperation::Reduce, subgroup_inclusive_xor, GroupOperation::InclusiveScan, subgroup_exclusive_xor, GroupOperation::ExclusiveScan; r"
A bitwise xor group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(impl Integer, "OpGroupNonUniformBitwiseXor", subgroup_clustered_xor; r"
A bitwise xor group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of integer type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

// logical and
macro_subgroup_op!(bool, "OpGroupNonUniformLogicalAnd", subgroup_logical_and, GroupOperation::Reduce, subgroup_inclusive_logical_and, GroupOperation::InclusiveScan, subgroup_exclusive_logical_and, GroupOperation::ExclusiveScan; r"
A logical and group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of Boolean type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is ~0.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(bool, "OpGroupNonUniformLogicalAnd", subgroup_clustered_logical_and; r"
A logical and group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of Boolean type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is ~0. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

// logical or
macro_subgroup_op!(bool, "OpGroupNonUniformLogicalOr", subgroup_logical_or, GroupOperation::Reduce, subgroup_inclusive_logical_or, GroupOperation::InclusiveScan, subgroup_exclusive_logical_or, GroupOperation::ExclusiveScan; r"
A logical or group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of Boolean type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(bool, "OpGroupNonUniformLogicalOr", subgroup_clustered_logical_or; r"
A logical or group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of Boolean type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

// logical xor
macro_subgroup_op!(bool, "OpGroupNonUniformLogicalXor", subgroup_logical_xor, GroupOperation::Reduce, subgroup_inclusive_logical_xor, GroupOperation::InclusiveScan, subgroup_exclusive_logical_xor, GroupOperation::ExclusiveScan; r"
A logical xor group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of Boolean type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0.

The type of `value` must be the same as Result Type.

Requires Capability `GroupNonUniformArithmetic`.
");
macro_subgroup_op_clustered!(bool, "OpGroupNonUniformLogicalXor", subgroup_clustered_logical_xor; r"
A logical xor group operation of all `value` operands contributed by active invocations in the group.

Result Type must be a scalar or vector of Boolean type.

Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.

The identity I for Operation is 0. If Operation is `ClusteredReduce`, `ClusterSize` must be present.

The type of `value` must be the same as Result Type.

`ClusterSize` is the size of cluster to use. `ClusterSize` must be a scalar of integer type, whose Signedness operand is 0. `ClusterSize` must come from a constant instruction. Behavior is undefined unless `ClusterSize` is at least 1 and a power of 2. If `ClusterSize` is greater than the size of the group, executing this instruction results in undefined behavior.

Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.

# Safety
* `ClusterSize` must not be greater than the size of the group
");

/// Result is the `value` of the invocation within the quad with a quad index equal to `index`.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope, but has no effect on the behavior of this instruction. It must be Subgroup.
///
/// The type of `value` must be the same as Result Type.
///
/// `index` must be a scalar of integer type, whose Signedness operand is 0.
///
/// Before version 1.5, `index` must come from a constant instruction. Starting with version 1.5, `index` must be dynamically uniform.
///
/// If the value of `index` is greater than or equal to 4, or refers to an inactive invocation, the resulting value is undefined.
///
/// Requires Capability `GroupNonUniformQuad`.
///
/// # Safety
/// * This function is safe
/// * Result is undefined if the value of `index` is greater than or equal to 4, or refers to an inactive invocation
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformQuadBroadcast")]
#[inline]
pub fn subgroup_quad_broadcast<T: ScalarOrVector>(value: T, index: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%index = OpLoad _ {index}",
            "%result = OpGroupNonUniformQuadBroadcast _ %subgroup %value %index",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            index = in(reg) &index,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Direction is the kind of swap to perform.
///
/// Direction must be a scalar of integer type, whose Signedness operand is 0.
///
/// Direction must come from a constant instruction.
///
/// The value returned in Result is the value provided to `value` by another invocation in the same quad scope instance. The invocation providing this `value` is determined according to Direction.
///
/// Requires Capability `GroupNonUniformQuad`.
pub enum QuadDirection {
    /// A Direction of 0 indicates a horizontal swap;
    /// - Invocations with quad indices of 0 and 1 swap values
    /// - Invocations with quad indices of 2 and 3 swap values
    Horizontal = 0,
    /// A Direction of 1 indicates a vertical swap;
    /// - Invocations with quad indices of 0 and 2 swap values
    /// - Invocations with quad indices of 1 and 3 swap values
    Vertical = 1,
    /// A Direction of 2 indicates a diagonal swap;
    /// - Invocations with quad indices of 0 and 3 swap values
    /// - Invocations with quad indices of 1 and 2 swap values
    Diagonal = 2,
}

/// Swap the `value` of the invocation within the quad with another invocation in the quad using Direction.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope, but has no effect on the behavior of this instruction. It must be Subgroup.
///
/// The type of `value` must be the same as Result Type.
///
/// Direction is the kind of swap to perform.
///
/// Direction must be a scalar of integer type, whose Signedness operand is 0.
///
/// Direction must come from a constant instruction.
///
/// The value returned in Result is the value provided to `value` by another invocation in the same quad scope instance. The invocation providing this `value` is determined according to Direction.
///
/// A Direction of 0 indicates a horizontal swap;
/// - Invocations with quad indices of 0 and 1 swap values
/// - Invocations with quad indices of 2 and 3 swap values
/// A Direction of 1 indicates a vertical swap;
/// - Invocations with quad indices of 0 and 2 swap values
/// - Invocations with quad indices of 1 and 3 swap values
/// A Direction of 2 indicates a diagonal swap;
/// - Invocations with quad indices of 0 and 3 swap values
/// - Invocations with quad indices of 1 and 2 swap values
///
/// Direction must be one of the above values.
///
/// If an active invocation reads `value` from an inactive invocation, the resulting value is undefined.
///
/// Requires Capability `GroupNonUniformQuad`.
///
/// # Safety
/// * This function is safe
/// * Result is undefined if an active invocation reads `value` from an inactive invocation
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformQuadSwap")]
#[inline]
pub fn subgroup_quad_swap<const DIRECTION: u32, T: ScalarOrVector>(value: T) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%direction = OpConstant %u32 {direction}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformQuadSwap _ %subgroup %value %direction",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            direction = const DIRECTION,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}
