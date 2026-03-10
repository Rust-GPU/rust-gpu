//! Intrinsics for task shaders

#[cfg(target_arch = "spirv")]
use core::arch::asm;

/// Defines the grid size of subsequent mesh shader workgroups to generate
/// upon completion of the task shader workgroup.
///
/// 'Group Count X Y Z' must each be a 32-bit unsigned integer value.
/// They configure the number of local workgroups in each respective dimensions
/// for the launch of child mesh tasks. See Vulkan API specification for more detail.
///
/// 'Payload' is an optional pointer to the payload structure to pass to the generated mesh shader invocations.
/// 'Payload' must be the result of an *`OpVariable`* with a storage class of *`TaskPayloadWorkgroupEXT`*.
///
/// The arguments are taken from the first invocation in each workgroup.
/// Any invocation must execute this instruction exactly once and under uniform
/// control flow.
/// This instruction also serves as an *OpControlBarrier* instruction, and also
/// performs and adheres to the description and semantics of an *OpControlBarrier*
/// instruction with the 'Execution' and 'Memory' operands set to *Workgroup* and
/// the 'Semantics' operand set to a combination of *`WorkgroupMemory`* and
/// *`AcquireRelease`*.
/// Ceases all further processing: Only instructions executed before
/// *`OpEmitMeshTasksEXT`* have observable side effects.
///
/// This instruction must be the last instruction in a block.
///
/// This instruction is only valid in the *`TaskEXT`* Execution Model.
///
/// # Safety
/// * Must be called **exactly once** in task shaders
/// * Must be called in uniform control flow
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpEmitMeshTasksEXT")]
#[inline]
pub unsafe fn emit_mesh_tasks_ext(group_count_x: u32, group_count_y: u32, group_count_z: u32) -> ! {
    unsafe {
        asm! {
            "OpEmitMeshTasksEXT {group_count_x} {group_count_y} {group_count_z}",
            group_count_x = in(reg) group_count_x,
            group_count_y = in(reg) group_count_y,
            group_count_z = in(reg) group_count_z,
            options(noreturn),
        }
    }
}

/// Defines the grid size of subsequent mesh shader workgroups to generate
/// upon completion of the task shader workgroup.
///
/// 'Group Count X Y Z' must each be a 32-bit unsigned integer value.
/// They configure the number of local workgroups in each respective dimensions
/// for the launch of child mesh tasks. See Vulkan API specification for more detail.
///
/// 'Payload' is an optional pointer to the payload structure to pass to the generated mesh shader invocations.
/// 'Payload' must be the result of an *`OpVariable`* with a storage class of *`TaskPayloadWorkgroupEXT`*.
///
/// The arguments are taken from the first invocation in each workgroup.
/// Any invocation must execute this instruction exactly once and under uniform
/// control flow.
/// This instruction also serves as an *OpControlBarrier* instruction, and also
/// performs and adheres to the description and semantics of an *OpControlBarrier*
/// instruction with the 'Execution' and 'Memory' operands set to *Workgroup* and
/// the 'Semantics' operand set to a combination of *`WorkgroupMemory`* and
/// *`AcquireRelease`*.
/// Ceases all further processing: Only instructions executed before
/// *`OpEmitMeshTasksEXT`* have observable side effects.
///
/// This instruction must be the last instruction in a block.
///
/// This instruction is only valid in the *`TaskEXT`* Execution Model.
///
/// # Safety
/// * Must be called **exactly once** in task shaders
/// * Must be called in uniform control flow
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpEmitMeshTasksEXT")]
#[inline]
pub unsafe fn emit_mesh_tasks_ext_payload<T>(
    group_count_x: u32,
    group_count_y: u32,
    group_count_z: u32,
    payload: &mut T,
) -> ! {
    unsafe {
        asm! {
            "OpEmitMeshTasksEXT {group_count_x} {group_count_y} {group_count_z} {payload}",
            group_count_x = in(reg) group_count_x,
            group_count_y = in(reg) group_count_y,
            group_count_z = in(reg) group_count_z,
            payload = in(reg) payload,
            options(noreturn),
        }
    }
}
