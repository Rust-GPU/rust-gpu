//! Provides the [`AccelerationStructure`] type and [`RayFlags`], as defined by the [`ray_pipeline`] and [`ray_query`]
//! extensions.
//!
//! Unlike the vulkan [`VK_KHR_acceleration_structure`] extension, there is no shared SPIR-V extension for acceleration
//! structures. Instead, both [`SPV_KHR_ray_tracing`] and [`SPV_KHR_ray_query`] specify (slightly adjusted):
//! > [`AccelerationStructure`], [`RayFlags`] and the `RayTraversalPrimitiveCullingKHR` capability are added by both
//! > [`ray_pipeline`] and [`ray_query`]; they are intended to have identical definitions, and can be enabled by either
//! > extension’s capability, for use with the instructions under that same capability.
//!
//! [`ray_pipeline`]: super::ray_pipeline
//! [`ray_query`]: super::ray_query
//! [`VK_KHR_acceleration_structure`]: https://docs.vulkan.org/refpages/latest/refpages/source/VK_KHR_acceleration_structure.html
//! [`SPV_KHR_ray_tracing`]: https://github.khronos.org/SPIRV-Registry/extensions/KHR/SPV_KHR_ray_tracing.html
//! [`SPV_KHR_ray_query`]: https://github.khronos.org/SPIRV-Registry/extensions/KHR/SPV_KHR_ray_query.html

#[cfg(target_arch = "spirv")]
use core::arch::asm;
use glam::UVec2;

/// An acceleration structure type which is an opaque reference to an
/// acceleration structure handle as defined in the client API specification.
#[spirv(acceleration_structure)]
#[derive(Copy, Clone)]
// HACK(eddyb) avoids "transparent newtype of `_anti_zst_padding`" misinterpretation.
#[repr(C)]
pub struct AccelerationStructure {
    // HACK(eddyb) avoids the layout becoming ZST (and being elided in one way
    // or another, before `#[spirv(acceleration_structure)]` can special-case it).
    _anti_zst_padding: core::mem::MaybeUninit<u32>,
}

impl AccelerationStructure {
    /// Converts a 64-bit integer into an [`AccelerationStructure`].
    /// # Safety
    /// The 64-bit integer must point to a valid acceleration structure.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpConvertUToAccelerationStructureKHR")]
    #[inline]
    pub unsafe fn from_u64(id: u64) -> AccelerationStructure {
        unsafe {
            // FIXME(eddyb) `let mut result = T::default()` uses (for `asm!`), with this.
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%ret = OpTypeAccelerationStructureKHR",
                "%result = OpConvertUToAccelerationStructureKHR %ret {id}",
                "OpStore {result_slot} %result",
                id = in(reg) id,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }

    /// Converts a vector of two 32 bit integers into an [`AccelerationStructure`].
    /// # Safety
    /// The combination must point to a valid acceleration structure.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpConvertUToAccelerationStructureKHR")]
    #[inline]
    pub unsafe fn from_vec(id: UVec2) -> AccelerationStructure {
        unsafe {
            // FIXME(eddyb) `let mut result = T::default()` uses (for `asm!`), with this.
            let mut result_slot = core::mem::MaybeUninit::uninit();
            asm! {
                "%ret = OpTypeAccelerationStructureKHR",
                "%id = OpLoad _ {id}",
                "%result = OpConvertUToAccelerationStructureKHR %ret %id",
                "OpStore {result_slot} %result",
                id = in(reg) &id,
                result_slot = in(reg) result_slot.as_mut_ptr(),
            }
            result_slot.assume_init()
        }
    }
}

bitflags::bitflags! {
    /// Flags controlling the properties of an OpTraceRayKHR instruction.
    /// Despite being a mask and allowing multiple bits to be combined, it is
    /// invalid for more than one of these four bits to be set: `OPAQUE`,
    /// `NO_OPAQUE`, `CULL_OPAQUE`, `CULL_NO_OPAQUE`, only one of
    /// `CULL_BACK_FACING_TRIANGLES` and `CULL_FRONT_FACING_TRIANGLES` may
    /// be set.
    #[repr(transparent)]
    #[cfg_attr(feature = "bytemuck", derive(bytemuck::Zeroable, bytemuck::Pod))]
    pub struct RayFlags: u32 {
        /// No flags specified.
        const NONE = 0;
        /// Force all intersections with the trace to be opaque.
        const OPAQUE = 1;
        /// Force all intersections with the trace to be non-opaque.
        const NO_OPAQUE = 2;
        /// Accept the first hit discovered.
        const TERMINATE_ON_FIRST_HIT = 4;
        /// Do not execute a closest hit shader.
        const SKIP_CLOSEST_HIT_SHADER = 8;
        /// Do not intersect with the back face of triangles.
        const CULL_BACK_FACING_TRIANGLES = 16;
        /// Do not intersect with the front face of triangles.
        const CULL_FRONT_FACING_TRIANGLES = 32;
        /// Do not intersect with opaque geometry.
        const CULL_OPAQUE = 64;
        /// Do not intersect with non-opaque geometry.
        const CULL_NO_OPAQUE = 128;
        /// Do not intersect with any triangle geometries.
        /// Requires `RayTraversalPrimitiveCullingKHR`.
        const SKIP_TRIANGLES = 256;
        /// Do not intersect with any AABB (Axis Aligned Bounding Box) geometries.
        /// Requires `RayTraversalPrimitiveCullingKHR`.
        const SKIP_AABBS = 512;
    }
}
