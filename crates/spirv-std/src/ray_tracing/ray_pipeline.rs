//! Intrinsics for ray tracing **pipelines**, for ray queries see [`super::ray_query`].
//!
//! Provides intrinsics as defined by the [`SPV_KHR_ray_tracing`] extension, used by the [`VK_KHR_ray_tracing_pipeline`]
//! Vulkan extension.
//!
//! [`SPV_KHR_ray_tracing`]: https://github.khronos.org/SPIRV-Registry/extensions/KHR/SPV_KHR_ray_tracing.html
//! [`VK_KHR_ray_tracing_pipeline`]: https://docs.vulkan.org/refpages/latest/refpages/source/VK_KHR_ray_tracing_pipeline.html

use crate::ray_tracing::{AccelerationStructure, RayFlags};
#[cfg(target_arch = "spirv")]
use core::arch::asm;
use glam::Vec3;

/// Trace a ray into the acceleration structure.
///
/// - `structure` is the descriptor for the acceleration structure to trace into.
/// - `ray_flags` contains one or more of the Ray Flag values.
/// - `cull_mask` is the mask to test against the instance mask. Only the 8
///   least-significant bits of are used by this instruction - other bits
///   are ignored.
/// - `sbt_offset` and `sbt_stride` control indexing into the SBT (Shader
///   Binding Table) for hit shaders called from this trace. Only the 4
///   least-significant bits of `sbt_offset` and `sbt_stride` are used by this
///   instruction - other bits are ignored.
/// - `miss_index` is the index of the miss shader to be called from this
///   trace call. Only the 16 least-significant bits are used by this
///   instruction - other bits are ignored.
/// - `ray_origin`, `ray_tmin`, `ray_direction`, and `ray_tmax` control the
///   basic parameters of the ray to be traced.
///
/// - `payload` is a pointer to the ray payload structure to use for this trace.
///   `payload` must have a storage class of `ray_payload`
///   or `incoming_ray_payload`.
///
/// This instruction is allowed only in `ray_generation`, `closest_hit` and
/// `miss` execution models.
///
/// This instruction is a shader call instruction which may invoke shaders with
/// the `intersection`, `any_hit`, `closest_hit`, and `miss`
/// execution models.
#[doc(alias = "OpTraceRayKHR")]
#[inline]
#[allow(clippy::too_many_arguments)]
#[spirv_std_macros::gpu_only]
pub unsafe fn trace_ray<T>(
    acceleration_structure: AccelerationStructure,
    ray_flags: RayFlags,
    cull_mask: i32,
    sbt_offset: i32,
    sbt_stride: i32,
    miss_index: i32,
    ray_origin: Vec3,
    ray_tmin: f32,
    ray_direction: Vec3,
    ray_tmax: f32,
    payload: &mut T,
) {
    unsafe {
        asm! {
            "%acceleration_structure = OpLoad _ {acceleration_structure}",
            "%ray_origin = OpLoad _ {ray_origin}",
            "%ray_direction = OpLoad _ {ray_direction}",
            "OpTraceRayKHR \
            %acceleration_structure \
            {ray_flags} \
            {cull_mask} \
            {sbt_offset} \
            {sbt_stride} \
            {miss_index} \
            %ray_origin \
            {ray_tmin} \
            %ray_direction \
            {ray_tmax} \
            {payload}",
            acceleration_structure = in(reg) &acceleration_structure,
            ray_flags = in(reg) ray_flags.bits(),
            cull_mask = in(reg) cull_mask,
            sbt_offset = in(reg) sbt_offset,
            sbt_stride = in(reg) sbt_stride,
            miss_index = in(reg) miss_index,
            ray_origin = in(reg) &ray_origin,
            ray_tmin = in(reg) ray_tmin,
            ray_direction = in(reg) &ray_direction,
            ray_tmax = in(reg) ray_tmax,
            payload = in(reg) payload,
        }
    }
}

/// Reports an intersection back to the traversal infrastructure.
///
/// If the intersection occurred within the current ray interval, the
/// intersection confirmation is performed (see the API specification for more
/// details). If the value of Hit falls outside the current ray interval, the
/// hit is rejected.
///
/// Returns True if the hit was accepted by the ray interval and the intersection was confirmed. Returns False otherwise.
///
/// - `hit` is the floating point parametric value along ray for the intersection.
/// - `hit_kind` is the integer hit kind reported back to other shaders and
///   accessible by the `hit kind` builtin.
///
/// This instruction is allowed only in `IntersectionKHR` execution model.
///
/// This instruction is a shader call instruction which may invoke shaders with
/// the `any_hit` execution model.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpReportIntersectionKHR")]
#[inline]
pub unsafe fn report_intersection(hit: f32, hit_kind: u32) -> bool {
    unsafe {
        let mut result = false;

        asm! {
            "%bool = OpTypeBool",
            "%result = OpReportIntersectionKHR %bool {hit} {hit_kind}",
            "OpStore {result} %result",
            result = in(reg) &mut result,
            hit = in(reg) hit,
            hit_kind = in(reg) hit_kind,
        };

        result
    }
}

/// Ignores the current potential intersection, terminating the invocation that
/// executes it, and continues the ray traversal.  This instruction is allowed
/// only in `any_hit` execution model.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpIgnoreIntersectionKHR")]
#[inline]
pub unsafe fn ignore_intersection() -> ! {
    unsafe {
        asm!("OpIgnoreIntersectionKHR", options(noreturn));
    }
}

/// Terminates the invocation that executes it, stops the ray traversal, accepts
/// the current hit, and invokes the `closest_hit` execution model
/// (if active). This instruction is allowed only in the `any_hit`
/// execution model.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpTerminateRayKHR")]
#[inline]
pub unsafe fn terminate_ray() -> ! {
    unsafe {
        asm!("OpTerminateRayKHR", options(noreturn));
    }
}

/// Invoke a callable shader.
///
/// - `INDEX` is the index into the SBT table to select callable shader
///   to execute.
/// - `data` is a pointer to the callable data to pass into the called shader.
///   `data` must have a storage class of `callable_data`
///   or `incoming_callable_data`.
///
/// This instruction is allowed only in `ray_generation`, `closest_hit`,
/// `miss` and `callable` execution models.
///
/// This instruction is a shader call instruction which will invoke a shader
/// with the `callable` execution model.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpExecuteCallableKHR")]
#[inline]
pub unsafe fn execute_callable<T, const ID: usize>(data: &T) {
    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%id = OpConstant %u32 {id}",
            "OpExecuteCallableKHR %id {data}",
            id = const ID,
            data = in(reg) data,
        };
    }
}
