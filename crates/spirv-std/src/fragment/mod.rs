//! Intrinsics for fragment shaders

#[cfg(target_arch = "spirv")]
use core::arch::asm;

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
#[allow(clippy::empty_loop)]
pub fn kill() -> ! {
    unsafe { asm!("OpKill", options(noreturn)) }
}
