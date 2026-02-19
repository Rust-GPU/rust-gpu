//! Symbols to query SPIR-V read-only global built-ins

/// compute shader built-ins
pub mod compute {
    #[cfg(target_arch = "spirv")]
    use core::arch::asm;
    use glam::UVec3;

    /// GLSL: `gl_LocalInvocationID()`
    /// WGSL: `local_invocation_id`
    #[doc(alias = "gl_LocalInvocationID")]
    #[inline]
    #[gpu_only]
    pub fn local_invocation_id() -> UVec3 {
        unsafe {
            let result = UVec3::default();
            asm! {
                "%builtin = OpVariable typeof{result} Input",
                "OpDecorate %builtin BuiltIn LocalInvocationId",
                "%result = OpLoad typeof*{result} %builtin",
                "OpStore {result} %result",
                result = in(reg) &result,
            }
            result
        }
    }
}
