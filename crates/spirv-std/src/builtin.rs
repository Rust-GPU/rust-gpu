//! Query SPIR-V read-only global built-in values
//!
//! Reference links:
//! * [WGSL specification describing these builtins](https://www.w3.org/TR/WGSL/#builtin-inputs-outputs)
//! * [SPIR-V specification for builtins](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
//! * [GLSL 4.x reference](https://registry.khronos.org/OpenGL-Refpages/gl4/)

/// Compute shader built-ins
pub mod compute {
    #[cfg(target_arch = "spirv")]
    use core::arch::asm;
    use glam::UVec3;

    // Local builtins (for this invocation's position in the workgroup).

    /// The current invocation’s local invocation ID,
    /// i.e. its position in the workgroup grid.
    ///
    /// GLSL: `gl_LocalInvocationID`
    /// WGSL: `local_invocation_id`
    #[doc(alias = "gl_LocalInvocationID")]
    #[inline]
    #[gpu_only]
    pub fn local_invocation_id() -> UVec3 {
        unsafe {
            let mut result = UVec3::default();
            asm! {
                "%builtin = OpVariable typeof{result_ref} Input",
                "OpDecorate %builtin BuiltIn LocalInvocationId",
                "%result = OpLoad typeof*{result_ref} %builtin",
                "OpStore {result_ref} %result",
                result_ref = in(reg) &mut result,
            }
            result
        }
    }

    /// The current invocation’s local invocation index,
    /// a linearized index of the invocation’s position within the workgroup grid.
    ///
    /// GLSL: `gl_LocalInvocationIndex`
    /// WGSL: `local_invocation_index`
    #[doc(alias = "gl_LocalInvocationIndex")]
    #[inline]
    #[gpu_only]
    pub fn local_invocation_index() -> u32 {
        unsafe {
            let result: u32;
            asm! {
                "%builtin_t = OpTypePointer Generic typeof{result}",
                "%builtin = OpVariable %builtin_t Input",
                "OpDecorate %builtin BuiltIn LocalInvocationIndex",
                "{result} = OpLoad typeof{result} %builtin",
                result = out(reg) result,
            }
            result
        }
    }

    // Global builtins, for this invocation's position in the compute grid.

    /// The current invocation’s global invocation ID,
    /// i.e. its position in the compute shader grid.
    ///
    /// GLSL: `gl_GlobalInvocationID`
    /// WGSL: `global_invocation_id`
    #[doc(alias = "gl_GlobalInvocationID")]
    #[inline]
    #[gpu_only]
    pub fn global_invocation_id() -> UVec3 {
        unsafe {
            let mut result = UVec3::default();
            asm! {
                "%builtin = OpVariable typeof{result_ref} Input",
                "OpDecorate %builtin BuiltIn GlobalInvocationId",
                "%result = OpLoad typeof*{result_ref} %builtin",
                "OpStore {result_ref} %result",
                result_ref = in(reg) &mut result,
            }
            result
        }
    }

    // Subgroup builtins

    ///  The number of subgroups in the current invocation’s workgroup.
    ///
    /// WGSL: `num_subgroups`
    /// No equivalent in GLSL.
    #[inline]
    #[gpu_only]
    pub fn num_subgroups() -> u32 {
        unsafe {
            let result: u32;
            asm! {
                "%builtin_t = OpTypePointer Generic typeof{result}",
                "%builtin = OpVariable %builtin_t Input",
                "OpDecorate %builtin BuiltIn NumSubgroups",
                "{result} = OpLoad typeof{result} %builtin",
                result = out(reg) result,
            }
            result
        }
    }

    /// The subgroup ID of current invocation’s subgroup within the workgroup.
    ///
    /// WGSL: `subgroup_id`
    /// No equivalent in GLSL.
    #[inline]
    #[gpu_only]
    pub fn subgroup_id() -> u32 {
        unsafe {
            let result: u32;
            asm! {
                "%builtin_t = OpTypePointer Generic typeof{result}",
                "%builtin = OpVariable %builtin_t Input",
                "OpDecorate %builtin BuiltIn SubgroupId",
                "{result} = OpLoad typeof{result} %builtin",
                result = out(reg) result,
            }
            result
        }
    }

    /// This invocation's ID within its subgroup.
    ///
    /// WGSL: `subgroup_invocation_id`
    /// No equivalent in GLSL.
    #[doc(alias = "subgroup_invocation_id")]
    #[inline]
    #[gpu_only]
    pub fn subgroup_local_invocation_id() -> u32 {
        unsafe {
            let result: u32;
            asm! {
                "%builtin_t = OpTypePointer Generic typeof{result}",
                "%builtin = OpVariable %builtin_t Input",
                "OpDecorate %builtin BuiltIn SubgroupLocalInvocationId",
                "{result} = OpLoad typeof{result} %builtin",
                result = out(reg) result,
            }
            result
        }
    }

    ///  The subgroup size of current invocation’s subgroup.
    ///
    /// WGSL: `subgroup_size`
    /// No equivalent in GLSL.
    #[inline]
    #[gpu_only]
    pub fn subgroup_size() -> u32 {
        unsafe {
            let result: u32;
            asm! {
                "%builtin_t = OpTypePointer Generic typeof{result}",
                "%builtin = OpVariable %builtin_t Input",
                "OpDecorate %builtin BuiltIn SubgroupSize",
                "{result} = OpLoad typeof{result} %builtin",
                result = out(reg) result,
            }
            result
        }
    }

    // Workgroup builtins

    /// The number of workgroups that have been dispatched in the compute shader grid.
    ///
    /// GLSL: `gl_NumWorkGroups`
    /// WGSL: `num_workgroups`
    #[doc(alias = "gl_WorkGroupID")]
    #[inline]
    #[gpu_only]
    pub fn num_workgroups() -> UVec3 {
        unsafe {
            let mut result = UVec3::default();
            asm! {
                "%builtin = OpVariable typeof{result_ref} Input",
                "OpDecorate %builtin BuiltIn NumWorkgroups",
                "%result = OpLoad typeof*{result_ref} %builtin",
                "OpStore {result_ref} %result",
                result_ref = in(reg) &mut result,
            }
            result
        }
    }

    /// The current invocation’s workgroup ID,
    /// i.e. the position of the workgroup in the overall compute shader grid.
    ///
    /// GLSL: `gl_WorkGroupID`
    /// WGSL: `workgroup_id`
    #[doc(alias = "gl_WorkGroupID")]
    #[inline]
    #[gpu_only]
    pub fn workgroup_id() -> UVec3 {
        unsafe {
            let mut result = UVec3::default();
            asm! {
                "%builtin = OpVariable typeof{result_ref} Input",
                "OpDecorate %builtin BuiltIn WorkgroupId",
                "%result = OpLoad typeof*{result_ref} %builtin",
                "OpStore {result_ref} %result",
                result_ref = in(reg) &mut result,
            }
            result
        }
    }
}
