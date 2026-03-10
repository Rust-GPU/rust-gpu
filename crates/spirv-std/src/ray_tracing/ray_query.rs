//! Intrinsics for ray **query**, for ray pipelines see [`super::ray_pipeline`].
//!
//! Provides intrinsics as defined by the [`SPV_KHR_ray_query`] extension, used by the [`VK_KHR_ray_query`]
//! Vulkan extension.
//!
//! [`SPV_KHR_ray_query`]: https://github.khronos.org/SPIRV-Registry/extensions/KHR/SPV_KHR_ray_query.html
//! [`VK_KHR_ray_query`]: https://docs.vulkan.org/refpages/latest/refpages/source/VK_KHR_ray_query.html

use crate::ray_tracing::matrix::Matrix4x3;
use crate::ray_tracing::{AccelerationStructure, RayFlags};
#[cfg(target_arch = "spirv")]
use core::arch::asm;
use glam::{Vec2, Vec3};

/// Describes the type of the intersection which is currently the candidate in a ray query,
/// returned by [`RayQuery::get_candidate_intersection_type`].
#[repr(u32)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::upper_case_acronyms)]
pub enum CandidateIntersection {
    /// A potential intersection with a triangle is being considered.
    Triangle = 0,
    /// A potential intersection with an axis-aligned bounding box is being considered.
    AABB = 1,
}

/// Describes the type of the intersection currently committed in a ray query, returned by
/// [`RayQuery::get_committed_intersection_type`].
#[repr(u32)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CommittedIntersection {
    /// No intersection is committed.
    None = 0,
    /// An intersection with a triangle has been committed.
    Triangle = 1,
    /// A user-generated intersection has been committed.
    Generated = 2,
}

/// A ray query type which is an opaque object representing a ray traversal.
#[spirv(ray_query)]
// HACK(eddyb) avoids "transparent newtype of `_anti_zst_padding`" misinterpretation.
#[repr(C)]
// HACK(eddyb) false positive due to `rustc` not understanding e.g. `ray_query!`.
#[allow(dead_code)]
pub struct RayQuery {
    // HACK(eddyb) avoids the layout becoming ZST (and being elided in one way
    // or another, before `#[spirv(ray_query)]` can special-case it).
    _anti_zst_padding: core::mem::MaybeUninit<u32>,
}

/// Constructs an uninitialized ray query variable. Using the syntax
/// `let (mut)? <name>`. Where `name` is the name of the ray query variable.
#[macro_export]
macro_rules! ray_query {
    (let $name:ident) => {
        $crate::ray_query!(@inner $name)
    };
    (let mut $name:ident) => {
        $crate::ray_query!(@inner $name, mut)
    };
    (@inner $name:ident $(, $mut:tt)?) => {
        let $name: &$($mut)? RayQuery = unsafe {
            let $name : *mut RayQuery;
            ::core::arch::asm! {
                "%ray_query = OpTypeRayQueryKHR",
                "%ray_query_ptr = OpTypePointer Generic %ray_query",
                "{name} = OpVariable %ray_query_ptr Function",
                name = out(reg) $name,
            }

            &$($mut)? *$name
        };
    }
}

impl RayQuery {
    /// Initialize a ray query object, defining parameters of traversal. After this
    /// call, a new ray trace can be performed with [`Self::proceed`]. Any
    /// previous traversal state stored in the object is lost.
    ///
    /// - `ray_query` is a pointer to the ray query to initialize.
    /// - `acceleration_structure` is the descriptor for the acceleration structure
    ///   to trace into.
    /// - `ray_flags` contains one or more of the Ray Flag values.
    /// - `cull_mask` is the mask to test against the instance mask.  Only the 8
    ///   least-significant bits of `cull_mask` are used by this instruction - other
    ///   bits are ignored.
    /// - `ray_origin`, `ray_tmin`, `ray_direction`, and `ray_tmax` control the
    ///   basic parameters of the ray to be traced.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryInitializeKHR")]
    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub unsafe fn initialize(
        &mut self,
        acceleration_structure: &AccelerationStructure,
        ray_flags: RayFlags,
        cull_mask: u32,
        ray_origin: Vec3,
        ray_tmin: f32,
        ray_direction: Vec3,
        ray_tmax: f32,
    ) {
        unsafe {
            asm! {
                "%acceleration_structure = OpLoad _ {acceleration_structure}",
                "%origin = OpLoad _ {ray_origin}",
                "%direction = OpLoad _ {ray_direction}",
                "OpRayQueryInitializeKHR \
                    {ray_query} \
                    %acceleration_structure \
                    {ray_flags} \
                    {cull_mask} \
                    %origin \
                    {ray_tmin} \
                    %direction \
                    {ray_tmax}",
                ray_query = in(reg) self,
                acceleration_structure = in(reg) acceleration_structure,
                ray_flags = in(reg) ray_flags.bits(),
                cull_mask = in(reg) cull_mask,
                ray_origin = in(reg) &ray_origin,
                ray_tmin = in(reg) ray_tmin,
                ray_direction = in(reg) &ray_direction,
                ray_tmax = in(reg) ray_tmax,
            }
        }
    }

    /// Allow traversal to proceed. Returns `true` if traversal is incomplete,
    /// and `false` when it has completed. A previous call to [`Self::proceed`]
    /// with the same ray query object must not have already returned `false`.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryProceedKHR")]
    #[inline]
    pub unsafe fn proceed(&self) -> bool {
        unsafe {
            let mut result = false;

            asm! {
                "%bool = OpTypeBool",
                "%result = OpRayQueryProceedKHR %bool {ray_query}",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Terminates further execution of a ray query; further calls to
    /// [`Self::proceed`] will return `false`. The value returned by any prior
    /// execution of [`Self::proceed`] with the same ray query object must have
    /// been true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryTerminateKHR")]
    #[inline]
    pub unsafe fn terminate(&self) {
        unsafe { asm!("OpRayQueryTerminateKHR {}", in(reg) self) }
    }

    /// Confirms a triangle intersection to be included in the determination
    /// of the closest hit for a ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must
    /// have returned true. The current intersection candidate must have a
    /// [`Self::get_candidate_intersection_type()`] of
    /// [`CandidateIntersection::Triangle`].
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryConfirmIntersectionKHR")]
    #[inline]
    pub unsafe fn confirm_intersection(&self) {
        unsafe { asm!("OpRayQueryConfirmIntersectionKHR {}", in(reg) self) }
    }

    /// Returns the type of the current candidate intersection.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionTypeKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_type(&self) -> CandidateIntersection {
        unsafe {
            let result: u32;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "{result} = OpRayQueryGetIntersectionTypeKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            #[allow(clippy::match_same_arms)]
            match result {
                0 => CandidateIntersection::Triangle,
                1 => CandidateIntersection::AABB,
                _ => CandidateIntersection::Triangle,
            }
        }
    }

    /// Returns the type of the current candidate intersection.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionTypeKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_type(&self) -> CommittedIntersection {
        unsafe {
            let result: u32;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "{result} = OpRayQueryGetIntersectionTypeKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            #[allow(clippy::match_same_arms)]
            match result {
                0 => CommittedIntersection::None,
                1 => CommittedIntersection::Triangle,
                2 => CommittedIntersection::Generated,
                _ => CommittedIntersection::None,
            }
        }
    }

    /// Returns the "Ray Tmin" value used by the ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetRayTMinKHR")]
    #[inline]
    pub unsafe fn get_ray_t_min(&self) -> f32 {
        unsafe {
            let result;

            asm! {
                "%f32 = OpTypeFloat 32",
                "{result} = OpRayQueryGetRayTMinKHR %f32 {ray_query}",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Returns the "Ray Flags" value used by the ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetRayFlagsKHR")]
    #[inline]
    pub unsafe fn get_ray_flags(&self) -> RayFlags {
        unsafe {
            let result;

            asm! {
                "{result} = OpRayQueryGetRayFlagsKHR typeof{result} {ray_query}",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            RayFlags::from_bits_truncate(result)
        }
    }

    /// Gets the "T" value for the current or previous intersection considered
    /// in a ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    /// The current intersection candidate must have a [`Self::get_candidate_intersection_type()`]
    /// of [`CandidateIntersection::Triangle`].
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionTKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_t(&self) -> f32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "{result} = OpRayQueryGetIntersectionTKHR typeof{result} {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the "T" value for the current or previous intersection considered
    /// in a ray query.
    ///
    /// There must be a current committed intersection.
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionTKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_t(&self) -> f32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "{result} = OpRayQueryGetIntersectionTKHR typeof{result} {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the custom index of the instance for the current intersection
    /// considered in a ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionInstanceCustomIndexKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_instance_custom_index(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "{result} = OpRayQueryGetIntersectionInstanceCustomIndexKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the custom index of the instance for the current intersection
    /// considered in a ray query.
    ///
    /// There must be a current committed intersection.
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionInstanceCustomIndexKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_instance_custom_index(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "{result} = OpRayQueryGetIntersectionInstanceCustomIndexKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the id of the instance for the current intersection considered in a
    /// ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionInstanceIdKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_instance_id(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "{result} = OpRayQueryGetIntersectionInstanceIdKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the id of the instance for the current intersection considered in a
    /// ray query.
    ///
    /// There must be a current committed intersection.
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionInstanceIdKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_instance_id(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "{result} = OpRayQueryGetIntersectionInstanceIdKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the shader binding table record offset for the current intersection
    /// considered in a ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_shader_binding_table_record_offset(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "{result} = OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the shader binding table record offset for the current intersection
    /// considered in a ray query.
    ///
    /// There must be a current committed intersection.
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_shader_binding_table_record_offset(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "{result} = OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the geometry index for the current intersection considered in a
    /// ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionGeometryIndexKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_geometry_index(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "{result} = OpRayQueryGetIntersectionGeometryIndexKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the geometry index for the current intersection considered in a
    /// ray query.
    ///
    /// There must be a current committed intersection.
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionGeometryIndexKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_geometry_index(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "{result} = OpRayQueryGetIntersectionGeometryIndexKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the primitive index for the current intersection considered in a
    /// ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionPrimitiveIndexKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_primitive_index(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "{result} = OpRayQueryGetIntersectionPrimitiveIndexKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the primitive index for the current intersection considered in a
    /// ray query.
    ///
    /// There must be a current committed intersection.
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionPrimitiveIndexKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_primitive_index(&self) -> u32 {
        unsafe {
            let result;

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "{result} = OpRayQueryGetIntersectionPrimitiveIndexKHR %u32 {ray_query} %intersection",
                ray_query = in(reg) self,
                result = out(reg) result,
            }

            result
        }
    }

    /// Gets the second and third barycentric coordinates of the current
    /// intersection considered in a ray query against the primitive it hit.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    /// The current intersection candidate must have a [`Self::get_candidate_intersection_type()`]
    /// of [`CandidateIntersection::Triangle`].
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionBarycentricsKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_barycentrics(&self) -> Vec2 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "%result = OpRayQueryGetIntersectionBarycentricsKHR typeof*{result} {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets the second and third barycentric coordinates of the current
    /// intersection considered in a ray query against the primitive it hit.
    ///
    /// There must be a current committed intersection. Its
    /// [`Self::get_committed_intersection_type()`] must be [`CommittedIntersection::Triangle`].
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionBarycentricsKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_barycentrics(&self) -> Vec2 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "%result = OpRayQueryGetIntersectionBarycentricsKHR typeof*{result} {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Returns whether the current intersection considered in a ray query was with
    /// the front face (`true`) or back face (`false`) of a primitive.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    /// The current intersection candidate must have a [`Self::get_candidate_intersection_type()`]
    /// of [`CandidateIntersection::Triangle`].
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionFrontFaceKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_front_face(&self) -> bool {
        unsafe {
            let mut result = false;

            asm! {
                "%bool = OpTypeBool",
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "%result = OpRayQueryGetIntersectionFrontFaceKHR %bool {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Returns whether the current intersection considered in a ray query was with
    /// the front face (`true`) or back face (`false`) of a primitive.
    ///
    /// There must be a current committed intersection. Its
    /// [`Self::get_committed_intersection_type()`] must be [`CommittedIntersection::Triangle`].
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionFrontFaceKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_front_face(&self) -> bool {
        unsafe {
            let mut result = false;

            asm! {
                "%bool = OpTypeBool",
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "%result = OpRayQueryGetIntersectionFrontFaceKHR %bool {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Returns whether a candidate intersection considered in a ray query was with
    /// an opaque AABB (Axis Aligned Bounding Box) or not.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionCandidateAABBOpaqueKHR")]
    #[inline]
    pub unsafe fn get_intersection_candidate_aabb_opaque(&self) -> bool {
        unsafe {
            let mut result = false;

            asm! {
                "%bool = OpTypeBool",
                "%result = OpRayQueryGetIntersectionCandidateAABBOpaqueKHR %bool {ray_query}",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets the object-space ray direction for the current intersection considered
    /// in a ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionObjectRayDirectionKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_object_ray_direction(&self) -> Vec3 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "%result = OpRayQueryGetIntersectionObjectRayDirectionKHR typeof*{result} {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets the object-space ray direction for the current intersection considered
    /// in a ray query.
    ///
    /// There must be a current committed intersection.
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionObjectRayDirectionKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_object_ray_direction(&self) -> Vec3 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "%result = OpRayQueryGetIntersectionObjectRayDirectionKHR typeof*{result} {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets the object-space ray origin for the current intersection considered in
    /// a ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionObjectRayOriginKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_object_ray_origin(&self) -> Vec3 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "%result = OpRayQueryGetIntersectionObjectRayOriginKHR typeof*{result} {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets the object-space ray origin for the current intersection considered in
    /// a ray query.
    ///
    /// There must be a current committed intersection.
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionObjectRayOriginKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_object_ray_origin(&self) -> Vec3 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "%result = OpRayQueryGetIntersectionObjectRayOriginKHR typeof*{result} {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets the world-space direction for the ray traced in a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetWorldRayDirectionKHR")]
    #[inline]
    pub unsafe fn get_world_ray_direction(&self) -> Vec3 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%result = OpRayQueryGetWorldRayDirectionKHR typeof*{result} {ray_query}",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets the world-space origin for the ray traced in a ray query.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetWorldRayOriginKHR")]
    #[inline]
    pub unsafe fn get_world_ray_origin(&self) -> Vec3 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%result = OpRayQueryGetWorldRayOriginKHR typeof*{result} {ray_query}",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets a matrix that transforms values to world-space from the object-space of
    /// the current intersection considered in a ray query.
    ///
    /// [`Self::proceed()`] must have been called on this object, and it must have returned true.
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionObjectToWorldKHR")]
    #[inline]
    pub unsafe fn get_candidate_intersection_object_to_world(&self) -> Matrix4x3 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "%result = OpRayQueryGetIntersectionObjectToWorldKHR typeof*{result} {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets a matrix that transforms values to world-space from the object-space of
    /// the current intersection considered in a ray query.
    ///
    /// There must be a current committed intersection.
    ///
    /// TODO: Improve docs. Can't right now due to
    /// <https://github.com/KhronosGroup/SPIRV-Registry/issues/128>
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionObjectToWorldKHR")]
    #[inline]
    pub unsafe fn get_committed_intersection_object_to_world(&self) -> Matrix4x3 {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 1",
                "%result = OpRayQueryGetIntersectionObjectToWorldKHR typeof*{result} {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }

    /// Gets the vertex positions for the triangle at the current intersection.
    ///
    /// Requires Capability `RayQueryPositionFetchKHR` and extension `SPV_KHR_ray_tracing_position_fetch`
    #[spirv_std_macros::gpu_only]
    #[doc(alias = "OpRayQueryGetIntersectionTriangleVertexPositionsKHR")]
    #[inline]
    pub unsafe fn get_intersection_triangle_vertex_positions(&self) -> [Vec3; 3] {
        unsafe {
            let mut result = Default::default();

            asm! {
                "%u32 = OpTypeInt 32 0",
                "%intersection = OpConstant %u32 0",
                "%result = OpRayQueryGetIntersectionTriangleVertexPositionsKHR typeof*{result} {ray_query} %intersection",
                "OpStore {result} %result",
                ray_query = in(reg) self,
                result = in(reg) &mut result,
            }

            result
        }
    }
}
