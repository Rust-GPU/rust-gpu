//! compute shader built-ins

use glam::UVec3;

/// The index of work item currently being operated on by a compute shader.
///
/// In the compute language, [`local_invocation_id`] is an input variable containing the n-dimensional index of the
/// local work invocation within the work group that the current shader is executing in. The possible values for this
/// variable range across the local work group size, i.e., `(0,0,0)` to
/// `(`[`workgroup_size`]`.x - 1, `[`workgroup_size`]`.y - 1, `[`workgroup_size`]`.z - 1)`.
///
/// * GLSL: [`gl_LocalInvocationID`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_LocalInvocationID.xhtml)
/// * WGSL: [`local_invocation_id`](https://www.w3.org/TR/WGSL/#local-invocation-id-builtin-value)
/// * SPIR-V: [`LocalInvocationId`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_LocalInvocationID")]
#[doc(alias = "LocalInvocationId")]
#[inline]
#[gpu_only]
pub fn local_invocation_id() -> UVec3 {
    crate::load_builtin!(LocalInvocationId)
}

/// The local linear index of work item currently being operated on by a compute shader.
///
/// In the compute language, [`local_invocation_index`] is a derived input variable containing the 1-dimensional
/// linearized index of the work invocation within the work group that the current shader is executing on. The value of
/// [`local_invocation_index`] is equal to [`local_invocation_id`]`.z * `[`workgroup_size`]`.x * `[`workgroup_size`]`.y`
/// `+ `[`local_invocation_id`]`.y * `[`workgroup_size`]`.x + `[`local_invocation_id`]`.x`.
///
/// * GLSL: [`gl_LocalInvocationIndex`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_LocalInvocationIndex.xhtml)
/// * WGSL: [`local_invocation_index`](https://www.w3.org/TR/WGSL/#local-invocation-index-builtin-value)
/// * SPIR-V: [`LocalInvocationIndex`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_LocalInvocationIndex")]
#[doc(alias = "LocalInvocationIndex")]
#[inline]
#[gpu_only]
pub fn local_invocation_index() -> u32 {
    crate::load_builtin!(LocalInvocationIndex)
}

/// The global index of work item currently being operated on by a compute shader.
///
/// In the compute language, [`global_invocation_id`] is a derived input variable containing the n-dimensional index of
/// the work invocation within the global work group that the current shader is executing on. The value of
/// [`global_invocation_id`] is equal to [`workgroup_id`]` * `[`workgroup_size`]` + `[`local_invocation_id`].
///
/// * GLSL: [`gl_GlobalInvocationID`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_GlobalInvocationID.xhtml)
/// * WGSL: [`global_invocation_id`](https://www.w3.org/TR/WGSL/#global-invocation-index-builtin-value)
/// * SPIR-V: [`GlobalInvocationId`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_GlobalInvocationID")]
#[doc(alias = "GlobalInvocationId")]
#[inline]
#[gpu_only]
pub fn global_invocation_id() -> UVec3 {
    crate::load_builtin!(GlobalInvocationId)
}

// custom: do not mention `glDispatchCompute` directly, be more general across APIs
/// The number of workgroups that have been dispatched to a compute shader.
///
/// In the compute language, [`num_workgroups`] contains the total number of work groups that will execute the compute
/// shader. The components of [`num_workgroups`] are equal to the `x`, `y`, and `z` parameters passed to the dispatch
/// command.
///
/// * GLSL: [`gl_NumWorkGroups`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_NumWorkGroups.xhtml)
/// * WGSL: [`num_workgroups`](https://www.w3.org/TR/WGSL/#num-workgroups-builtin-value)
/// * SPIR-V: [`NumWorkgroups`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_NumWorkGroups")]
#[doc(alias = "NumWorkgroups")]
#[inline]
#[gpu_only]
pub fn num_workgroups() -> UVec3 {
    crate::load_builtin!(NumWorkgroups)
}

// custom: do not mention `glDispatchCompute` directly, be more general across APIs
/// The index of the workgroup currently being operated on by a compute shader.
///
/// In the compute language, [`workgroup_id`] contains the 3-dimensional index of the global work group that the current
/// compute shader invocation is executing within. The possible values range across the parameters passed into the
/// dispatch command, i.e., from `(0, 0, 0)` to
/// `(`[`num_workgroups`]`.x - 1, `[`num_workgroups`]`.y - 1, `[`num_workgroups`]`.z - 1)`.
///
/// * GLSL: [`gl_WorkGroupID`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_WorkGroupID.xhtml)
/// * WGSL: [`workgroup_id`](https://www.w3.org/TR/WGSL/#workgroup-id-builtin-value)
/// * SPIR-V: [`WorkgroupId`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_WorkGroupID")]
#[doc(alias = "WorkgroupId")]
#[inline]
#[gpu_only]
pub fn workgroup_id() -> UVec3 {
    crate::load_builtin!(WorkgroupId)
}
