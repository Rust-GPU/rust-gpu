//! vertex shader built-ins
//!
//! Many of the built-ins are also available on the
//!
//! # Vulkan Index vs OpenGL ID
//!
//! A few built-ins have been given different names (and decoration ids) between Vulkan and OpenGL, see table below.
//! Rust-GPU exposes these built-ins under the Vulkan naming scheme of "index", see [`vertex_index`] or
//! [`instance_index`]. When using these built-ins on an OpenGL SPIR-V target, these intrinsics will use
//! the OpenGL variant of the built-in. Also, `gl_DrawID` is inconsistently named "id" on both platforms, so we're
//! calling it the [`draw_index`].
//!
//! | Vulkan           | OpenGL     |
//! |------------------|------------|
//! | gl_VertexIndex   | VertexID   |
//! | gl_InstanceIndex | InstanceId |
//! | gl_DrawID        | gl_DrawID  |
//!

use glam::Vec4;

// custom: vulkan vs opengl naming
/// [`vertex_index`] is a vertex language input variable that holds an integer index for the vertex, relative to a base.
///
/// On Vulkan targets, maps to [`VertexIndex`](https://docs.vulkan.org/glsl/latest/chapters/builtins.html#vertex-shader-special-variables).
/// On OpenGL targets, maps to [`VertexID`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_VertexID.xhtml).
/// Also see module-level documentation on [Vulkan Index vs OpenGL ID][index_id].
///
/// * Vulkan GLSL: [`gl_VertexIndex`](https://docs.vulkan.org/glsl/latest/chapters/builtins.html#vertex-shader-special-variables)
/// * OpenGL GLSL: [`gl_VertexID`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_VertexID.xhtml)
/// * WGSL: [`vertex_index`](https://www.w3.org/TR/WGSL/#vertex-index-builtin-value)
/// * Vulkan SPIR-V: [`VertexIndex`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
/// * OpenGL SPIR-V: [`VertexID`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
///
/// [index_id]: crate::arch::vertex#Vulkan-Index-vs-OpenGL-ID
#[doc(alias = "gl_VertexIndex")]
#[doc(alias = "gl_VertexID")]
#[doc(alias = "VertexIndex")]
#[doc(alias = "VertexID")]
#[inline]
#[gpu_only]
pub fn vertex_index() -> u32 {
    #[cfg(any(
        target_env = "opengl4.0",
        target_env = "opengl4.1",
        target_env = "opengl4.2",
        target_env = "opengl4.3",
        target_env = "opengl4.4",
        target_env = "opengl4.5"
    ))]
    {
        crate::load_builtin!(VertexId)
    }
    #[cfg(not(any(
        target_env = "opengl4.0",
        target_env = "opengl4.1",
        target_env = "opengl4.2",
        target_env = "opengl4.3",
        target_env = "opengl4.4",
        target_env = "opengl4.5"
    )))]
    {
        crate::load_builtin!(VertexIndex)
    }
}

// custom: vulkan vs opengl naming
/// [`instance_index`] is a vertex language input variable that holds the instance number of the current primitive in an
/// instanced draw call, relative to a base. If the current primitive does not come from an instanced draw call, the
/// value of [`instance_index`] is zero.
///
/// On Vulkan targets, maps to [`InstanceIndex`](https://docs.vulkan.org/glsl/latest/chapters/builtins.html#vertex-shader-special-variables).
/// On OpenGL targets, maps to [`InstanceID`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_PrimitiveID.xhtml).
/// Also see module-level documentation on [Vulkan Index vs OpenGL ID][index_id].
///
/// * Vulkan GLSL: [`gl_InstanceIndex`](https://docs.vulkan.org/glsl/latest/chapters/builtins.html#vertex-shader-special-variables)
/// * OpenGL GLSL: [`gl_InstanceID`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_InstanceID.xhtml)
/// * WGSL: [`instance_index`](https://www.w3.org/TR/WGSL/#instance-index-builtin-value)
/// * Vulkan SPIR-V: [`InstanceIndex`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
/// * OpenGL SPIR-V: [`InstanceID`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
///
/// [index_id]: crate::arch::vertex#Vulkan-Index-vs-OpenGL-ID
#[doc(alias = "gl_InstanceIndex")]
#[doc(alias = "gl_InstanceID")]
#[doc(alias = "InstanceIndex")]
#[doc(alias = "InstanceID")]
#[inline]
#[gpu_only]
pub fn instance_index() -> u32 {
    #[cfg(any(
        target_env = "opengl4.0",
        target_env = "opengl4.1",
        target_env = "opengl4.2",
        target_env = "opengl4.3",
        target_env = "opengl4.4",
        target_env = "opengl4.5"
    ))]
    {
        crate::load_builtin!(InstanceId)
    }
    #[cfg(not(any(
        target_env = "opengl4.0",
        target_env = "opengl4.1",
        target_env = "opengl4.2",
        target_env = "opengl4.3",
        target_env = "opengl4.4",
        target_env = "opengl4.5"
    )))]
    {
        crate::load_builtin!(InstanceIndex)
    }
}

// custom: glsl naming inconsistency
/// [`draw_index`] is a vertex shader input variable that holds the integer index of the drawing command to which the
/// current vertex belongs. If the vertex is not invoked by a Multi* form of a draw command, then the value of
/// [`draw_index`] is zero.
///
/// The glsl name is inconsistent: Unlike `gl_InstanceIndex` or `instance_index`, which got renamed from `ID` to
/// `Index` going from OpenGL to Vulkan glsl, `gl_DrawID` remains as an `ID`. We chose to rename it to an `index` to
/// stay consistent. Also see module-level documentation on [Vulkan Index vs OpenGL ID][index_id].
///
/// * GLSL: [`gl_DrawID`](https://docs.vulkan.org/glsl/latest/chapters/builtins.html#vertex-shader-special-variables)
/// * WGSL: None
/// * SPIR-V: [`DrawIndex`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
///
/// [index_id]: crate::arch::vertex#Vulkan-Index-vs-OpenGL-ID
#[doc(alias = "gl_DrawID")]
#[doc(alias = "gl_DrawIndex")]
#[doc(alias = "DrawID")]
#[doc(alias = "DrawIndex")]
#[inline]
#[gpu_only]
pub fn draw_index() -> u32 {
    crate::load_builtin!(DrawIndex)
}

/// [`base_vertex`] is a vertex shader input variable that holds the integer value passed to the `baseVertex` parameter
/// of the command that resulted in the current shader invocation.
///
/// * GLSL: [`gl_BaseVertex`](https://docs.vulkan.org/glsl/latest/chapters/builtins.html#vertex-shader-special-variables)
/// * WGSL: None
/// * SPIR-V: [`BaseVertex`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_BaseVertex")]
#[doc(alias = "BaseVertex")]
#[inline]
#[gpu_only]
pub fn base_vertex() -> u32 {
    crate::load_builtin!(BaseVertex)
}

/// [`base_instance`] is a vertex shader input variable that holds the integer value passed to the `baseInstance`
/// parameter of the command that resulted in the current shader invocation.
///
/// * GLSL: [`gl_BaseInstance`](https://docs.vulkan.org/glsl/latest/chapters/builtins.html#vertex-shader-special-variables)
/// * WGSL: None
/// * SPIR-V: [`BaseInstance`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_BaseInstance")]
#[doc(alias = "BaseVertex")]
#[inline]
#[gpu_only]
pub fn base_instance() -> u32 {
    crate::load_builtin!(BaseInstance)
}

/// TODO docs
#[doc(alias = "gl_Position")]
#[doc(alias = "Position")]
#[inline]
#[gpu_only]
pub fn decl_position_out() -> &'static mut Vec4 {
    crate::decl_builtin_output!(Position)
}
