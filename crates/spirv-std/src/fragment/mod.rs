//! Intrinsics for fragment shaders

#[cfg(target_arch = "spirv")]
use core::arch::asm;
use glam::{Vec2, Vec4};

mod demote_to_helper_invocation;
mod derivative;

pub use demote_to_helper_invocation::*;
pub use derivative::*;

/// Fragment-shader discard. Equivalent to `discard()` from GLSL
///
/// Ceases all further processing in any invocation that executes it: Only
/// instructions these invocations executed before [kill] have observable side
/// effects.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpKill", alias = "discard")]
pub fn kill() -> ! {
    unsafe { asm!("OpKill", options(noreturn)) }
}

/// The window-relative coordinates of the current fragment.
///
/// Available only in the fragment shaders, [`frag_coord`] is an input variable that contains the window relative
/// coordinate `(x, y, z, 1/w)` values for the fragment. If multi-sampling, this value can be for any location within
/// the pixel, or one of the fragment samples. This value is the result of fixed functionality that interpolates
/// primitives after vertex processing to generate fragments. The `z` component is the depth value that would be used
/// for the fragment's depth if no shader contained any writes to [`frag_coord`].
///
/// In rust-gpu, you can't declare the additional layout qualifier identifiers and this function operates as if
/// `pixel_center_integer` is declared. [`frag_coord`] assumes a lower-left origin for window coordinates and assumes
/// pixel centers are located at half-pixel centers. For example, the `(x, y)` location `(0.5, 0.5)` is returned for the
/// lower-left-most pixel in a window.
///
/// * GLSL: [`gl_FragCoord`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_FragCoord.xhtml)
/// * WGSL: [`position`](https://www.w3.org/TR/WGSL/#built-in-values-position)
/// * SPIR-V: [`FragCoord`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
///
/// # rust-gpu
///
/// No additional layout qualifier identifier can be set and frag coord is always declared as `pixel_center_integer`.
///
#[doc(alias = "gl_FragCoord")]
#[doc(alias = "FragCoord")]
#[inline]
#[gpu_only]
pub fn frag_coord() -> Vec4 {
    crate::load_builtin!(FragCoord)
}

/// Indicates whether a primitive is front or back facing.
///
/// Available only in the fragment shader, [`front_facing`] is an input variable whose value is true if the fragment
/// belongs to a front-facing primitive and false otherwise. The determination of whether a triangle primitive is
/// front-facing is made by examining the sign of the area of the triangle, including a possible reversal of this sign
/// as controlled by front face rasterizer configuration.
///
/// * GLSL: [`gl_FrontFacing`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_FrontFacing.xhtml)
/// * WGSL: [`front_facing`](https://www.w3.org/TR/WGSL/#front-facing-builtin-value)
/// * SPIR-V: [`FrontFacing`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_FrontFacing")]
#[doc(alias = "FrontFacing")]
#[inline]
#[gpu_only]
pub fn front_facing() -> bool {
    crate::load_builtin!(FrontFacing)
}

/// Provides a forward-compatible mechanism for vertex clipping.
///
/// The element [`clip_distance`]`[i]` specifies a clip distance for each user clip plane `i`. A distance of `0.0` means
/// that the  vertex is on the plane, a positive distance means that the vertex is inside the clip plane, and a negative
/// distance means that the point is outside the clip plane. The clip distances will be linearly interpolated across the
/// primitive and the portion of the primitive with interpolated distances less than `0.0` will be clipped.
///
/// The [`clip_distance`] array is initially predeclared as unsized and must be sized by the shader by redeclaring
/// it with an explicit size. The array must be sized to include all clip planes that are enabled via the API; if the
/// size does not include all enabled planes, results are undefined. The shader must also set all values in
/// [`clip_distance`] that have been enabled via the API, or results are undefined. Values written into
/// [`clip_distance`] planes that are not enabled have no effect.
///
/// * GLSL: [`gl_ClipDistance`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_ClipDistance.xhtml)
/// * WGSL: None
/// * SPIR-V: [`ClipDistance`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
///
/// # Safety
///
/// The returned array may contain undefined results if the associated clip planes are not enabled.
#[doc(alias = "gl_ClipDistance")]
#[doc(alias = "ClipDistance")]
#[inline]
#[gpu_only]
pub unsafe fn clip_distance<const N: usize>() -> [f32; N] {
    crate::load_builtin!(ClipDistance; default: [0.; N])
}

/// Provides a mechanism for controlling user culling.
///
/// The element [`cull_distance`]`[i]` specifies a cull distance for each plane `i`. A distance of `0.0` means that the
/// vertex is on the plane, a positive distance means that the vertex is inside the cull volume, and a negative distance
/// means that the point is outside the cull volume. Primitives whose vertices all have a negative clip distance for
/// plane `i` will be discarded.
///
/// The [`cull_distance`] array is predeclared as unsized and must be sized by the shader by redeclaring it with an
/// explicit size. The size determines the number and set of enabled cull distances. The number of varying components
/// consumed by [`cull_distance`] will match the size of the array. Shaders writing [`cull_distance`] must write all
/// enabled distances, or culling results are undefined.
///
/// In the fragment shader, the [`cull_distance`] array contains linearly interpolated values for the vertex values
/// written by a shader to the [`cull_distance`] vertex output variable.
///
/// * GLSL: [`gl_CullDistance`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_CullDistance.xhtml)
/// * WGSL: None
/// * SPIR-V: [`CullDistance`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
///
/// # Safety
///
/// The returned array may contain undefined results if the associated cull planes are not enabled.
#[doc(alias = "gl_CullDistance")]
#[doc(alias = "CullDistance")]
#[inline]
#[gpu_only]
pub unsafe fn cull_distance<const N: usize>() -> [f32; N] {
    crate::load_builtin!(CullDistance; default: [0.; N])
}

/// The coordinate of a fragment within a point.
///
/// [`point_coord`] is a fragment shader input variable that contains the two-dimensional coordinates indicating where
/// within a point primitive the current fragment is located. If the current primitive is not a point, then values read
/// from [`point_coord`] are undefined.
///
/// * GLSL: [`gl_PointCoord`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_PointCoord.xhtml)
/// * WGSL: None
/// * SPIR-V: [`PointCoord`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_PointCoord")]
#[doc(alias = "PointCoord")]
#[inline]
#[gpu_only]
pub fn point_coord() -> Vec2 {
    crate::load_builtin!(PointCoord)
}

/// The index of the current primitive.
///
/// [`primitive_id`] is a tessellation control, tessellation evaluation and fragment language input variable. For the
/// tessellation control and tessellation evaluation languages, it holds the number of primitives processed by the
/// shader since the current set of rendering primitives was started. The first primitive processed by the drawing
/// command is numbered zero and the primitive ID counter is incremented after every individual point, line or triangle
/// primitive is processed. For triangles drawn in point or line mode, the primitive ID counter is incremented only
/// once, even through multiple points or lines may actually be drawn. Restarting a primitive topology using the
/// primitive restart index has no effect on the primitive ID counter.
///
/// In the geometry language, [`primitive_id`] is an output variable that is passed to the corresponding
/// [`primitive_id`] input variable in the fragment shader. If no geomery shader is present then [`primitive_id`] in the
/// fragment language behaves identically as it would in the tessellation control and evaluation languages. If a
/// geometry shader is present but does not write to [`primitive_id`], the value of [`primitive_id`] in the fragment
/// shader is undefined.
///
/// # rust-gpu
///
/// Using a geometry shader and *not* writing the `primitive_id` *technically* makes the returned values in the fragment
/// shader undefined. But as geometry shader are disappearing from graphics, we decided to not declare this function
/// as unsafe. Unlike [`layer`] or [`viewport_index`], this builtin is useful outside of geometry shaders.
///
/// * GLSL: [`gl_PrimitiveID`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_PrimitiveID.xhtml)
/// * WGSL: [`primitive_index`](https://www.w3.org/TR/WGSL/#primitive-index-builtin-value)
/// * SPIR-V: [`PrimitiveId`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_PrimitiveID")]
#[doc(alias = "gl_PrimitiveIndex")]
#[doc(alias = "PrimitiveId")]
#[doc(alias = "PrimitiveIndex")]
#[inline]
#[gpu_only]
pub fn primitive_index() -> u32 {
    crate::load_builtin!(PrimitiveId; extra: "OpDecorate %builtin Flat")
}

/// The index of the sample currently being processed.
///
/// [`sample_index`] is a fragment shader input variable that contains the index of the sample currently being
/// processed. This variable is in the range `0` to `num_samples - 1`, where `num_samples - 1` is the total number of
/// samples in each fragment for the current framebuffer (and thus 1 if rendering to a non-multisample buffer). Any
/// static use of this variable in a fragment shader causes the entire shader to be evaluated per-sample rather than
/// per-fragment.
///
/// When rendering to a non-multisample buffer, or if multisample rasterization is disabled, [`sample_index`] will
/// always be zero.
///
/// * GLSL: [`gl_SampleID`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_SampleID.xhtml)
/// * WGSL: [`sample_index`](https://www.w3.org/TR/WGSL/#built-in-values-sample_index)
/// * SPIR-V: [`SampleId`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_SampleID")]
#[doc(alias = "gl_SampleIndex")]
#[doc(alias = "SampleId")]
#[doc(alias = "SampleIndex")]
#[inline]
#[gpu_only]
pub fn sample_index() -> u32 {
    crate::load_builtin!(SampleId; extra: "OpDecorate %builtin Flat")
}

/// The location of the current sample within the current fragment.
///
/// [`sample_position`] in a fragment shader input variable that contains the location within a fragment of the sample
/// currently being processed. The `x` and `y` components of [`sample_position`] contain the sub-pixel coordinate of the
/// current sample and will have values in the range `0.0` to `1.0`. The sub-pixel coordinates of the center of the
/// pixel are always `(0.5, 0.5)`. Any static use of [`sample_position`] causes the entire fragment shader to be
/// evaluated per-sample rather than per-fragment. When rendering to a non-multisample buffer, or if multisample
/// rasterization is disabled, gl_SamplePosition will be `(0.5, 0.5)`.
///
/// * GLSL: [`gl_SamplePosition`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_SamplePosition.xhtml)
/// * WGSL: None
/// * SPIR-V: [`SamplePosition`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_SamplePosition")]
#[doc(alias = "SamplePosition")]
#[inline]
#[gpu_only]
pub fn sample_position() -> Vec2 {
    crate::load_builtin!(SamplePosition)
}

/// The computed sample coverage mask for the current fragment.
///
/// [`sample_mask`] is a fragment shader input variable that indicates the set of samples covered by the primitive
/// generating the fragment during multisample rasterization. It has a sample bit set if and only if the sample is
/// considered covered for this fragment shader invocation. Bit `B` of mask [`sample_mask`]`[M]` corresponds to sample
/// `32 * M + B`. The array has `ceil(s / 32)` elements where `s` is the maximum number of color samples supported by
/// the implementation.
///
/// * GLSL: [`gl_SampleMaskIn`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_SampleMaskIn.xhtml)
/// * WGSL: [`sample_mask`](https://www.w3.org/TR/WGSL/#built-in-values-sample_mask)
/// * SPIR-V: [`SampleMask`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
///
/// # Safety
///
/// The length of the array is implementation defined, according to spec. Unsure how to correctly model this.
#[doc(alias = "gl_SampleMaskIn")]
#[doc(alias = "SampleId")]
#[inline]
#[gpu_only]
pub unsafe fn sample_mask<const N: usize>() -> [u32; N] {
    crate::load_builtin!(SampleMask; default: [0; N]; extra: "OpDecorate %builtin Flat")
}

// Requires geometry shader to be used, adding it anyway
/// The selected layer of a multi-layer framebuffer attachment.
///
/// The input variable [`layer`] in a fragment shader will have the same value that was written to the output variable
/// [`layer`] in the geometry language. If the geometry stage does not dynamically assign a value to [`layer`], the
/// value of [`layer`] in the fragment stage will be undefined. If the geometry stage makes no static assignment to
/// [`layer`], the input [`layer`] in the fragment stage will be zero. Otherwise, the fragment stage will read the same
/// value written by the geometry stage, even if that value is out of range.
///
/// * GLSL: [`gl_Layer`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_Layer.xhtml)
/// * WGSL: None
/// * SPIR-V: [`Layer`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
///
/// # Safety
///
/// Value is undefined when the geometry shader does not set it.
#[doc(alias = "gl_Layer")]
#[doc(alias = "Layer")]
#[inline]
#[gpu_only]
pub unsafe fn layer() -> u32 {
    crate::load_builtin!(Layer; extra: "OpDecorate %builtin Flat")
}

/// The index of the viewport to be used in viewport transformation and scissoring.
///
/// In a fragment shader, [`viewport_index`] will have the same value that was written to the output variable
/// `viewport_index` in the geometry stage. If the geometry stage does not dynamically assign to `viewport_index`, the
/// value of [`viewport_index`] in the fragment shader will be undefined. If the geometry stage makes no static
/// assignment to [`viewport_index`], the fragment stage will read zero. Otherwise, the fragment stage will read the
/// same value written by the geometry stage, even if that value is out of range. If a fragment shader contains static
/// access to [`viewport_index`], it will count against the implementation defined limit for the maximum number of
/// inputs to the fragment stage.
///
/// * GLSL: [`gl_ViewportIndex`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_ViewportIndex.xhtml)
/// * WGSL: None
/// * SPIR-V: [`ViewportIndex`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
#[doc(alias = "gl_ViewportIndex")]
#[doc(alias = "ViewportIndex")]
#[inline]
#[gpu_only]
pub unsafe fn viewport_index() -> u32 {
    crate::load_builtin!(ViewportIndex; extra: "OpDecorate %builtin Flat")
}

/// Indicates whether a fragment shader invocation is a helper invocation.
///
/// The value [`is_helper_invocation_builtin`] is true if the fragment shader invocation is considered a helper
/// invocation and is false otherwise. A helper invocation is a fragment-shader invocation that is created solely for
/// the purposes of evaluating derivatives for use in non-helper fragment-shader invocations. Such derivatives are
/// computed implicitly in any [`sample`] function, and explicitly in the derivative functions of [`Derivative`].
///
/// Fragment shader helper invocations execute the same shader code as non-helper invocations, but will not have side
/// effects that modify the framebuffer or other shader-accessible memory. In particular:
/// * Fragments corresponding to helper invocations are discarded when shader execution is complete, without updating
///   the framebuffer.
/// * Stores to image and buffer variables performed by helper invocations have no effect on the underlying image or
///   buffer memory.
/// * Atomic operations to image, buffer, or atomic counter variables performed by helper invocations have no effect on
///   the underlying image or buffer memory. The values returned by such atomic operations are undefined.
///
/// Helper invocations may be generated for pixels not covered by a primitive being rendered. While fragment shader
/// inputs qualified with centroid are normally required to be sampled in the intersection of the pixel and the
/// primitive, the requirement is ignored for such pixels since there is no intersection between the pixel and
/// primitive.
///
/// Helper invocations may also be generated for fragments that are covered by a primitive being rendered when the
/// fragment is killed by early fragment tests (using the early_fragment_tests qualifier) or where the implementation is
/// able to determine that executing the fragment shader would have no effect other than assisting in computing
/// derivatives for other fragment shader invocations.
///
/// The set of helper invocations generated when processing any set of primitives is implementation dependent.
///
/// * GLSL: [`gl_HelperInvocation`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_HelperInvocation.xhtml)
/// * WGSL: None
/// * SPIR-V: [`SampleMask`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
///
/// Also see [`demote_to_helper_invocation`] and [`is_helper_invocation`].
///
/// [`sample`]: `crate::image::Image::sample`
#[doc(alias = "gl_HelperInvocation")]
#[doc(alias = "HelperInvocation")]
#[inline]
#[gpu_only]
pub fn is_helper_invocation_builtin() -> bool {
    crate::load_builtin!(HelperInvocation)
}
