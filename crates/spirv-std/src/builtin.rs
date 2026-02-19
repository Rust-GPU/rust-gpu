//! Symbols to query SPIR-V read-only global built-ins

/// compute shader built-ins
pub mod compute {
    #[cfg(target_arch = "spirv")]
    use core::arch::asm;
    use glam::UVec3;

    /// GLSL: `gl_LocalInvocationID`
    /// WGSL: `local_invocation_id`
    #[doc(alias = "gl_LocalInvocationID")]
    #[inline]
    #[gpu_only]
    pub fn local_invocation_id() -> UVec3 {
        unsafe {
            let mut result = UVec3::default();
            asm! {
                "%builtin = OpVariable typeof{result} Input",
                "OpDecorate %builtin BuiltIn LocalInvocationId",
                "%result = OpLoad typeof*{result} %builtin",
                "OpStore {result} %result",
                result = in(reg) &mut result,
            }
            result
        }
    }

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
        #[cfg(false)]
        unsafe {
            let mut result = 0_u32;
            asm! {
                "%builtin = OpVariable typeof{result_ref} Input",
                "OpDecorate %builtin BuiltIn LocalInvocationIndex",
                "%result = OpLoad typeof*{result_ref} %builtin",
                "OpStore {result_ref} %result",
                result_ref = in(reg) &mut result,
            }
            result
        }
    }
}
