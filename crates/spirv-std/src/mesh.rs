//! Intrinsics for mesh shaders

#[cfg(target_arch = "spirv")]
use core::arch::asm;

/// Sets the actual output size of the primitives and vertices that the mesh shader
/// workgroup will emit upon completion.
///
/// 'Vertex Count' must be a 32-bit unsigned integer value.
/// It defines the array size of per-vertex outputs.
///
/// 'Primitive Count' must a 32-bit unsigned integer value.
/// It defines the array size of per-primitive outputs.
///
/// The arguments are taken from the first invocation in each workgroup.
/// Any invocation must execute this instruction no more than once and under
/// uniform control flow.
/// There must not be any control flow path to an output write that is not preceded
/// by this instruction.
///
/// This instruction is only valid in the *`MeshEXT`* Execution Model.
///
/// # Safety
/// * Must be called **exactly once** in mesh shaders
/// * Must be called in uniform control flow
/// * Must not write any output before this instruction in invoked
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpSetMeshOutputsEXT")]
#[inline]
pub unsafe fn set_mesh_outputs_ext(vertex_count: u32, primitive_count: u32) {
    unsafe {
        asm! {
            "OpSetMeshOutputsEXT {vertex_count} {primitive_count}",
            vertex_count = in(reg) vertex_count,
            primitive_count = in(reg) primitive_count,
        }
    }
}
