//! Functionality to declare builtins, mostly proc macros
//!
//! # Making built-in functions for `spirv-std`
//!
//! Usually, built-ins are implemented as freestanding functions in `spirv-std`. We like to keep function declaration
//! outside the macro to make it easier for users to browse the source code.
//!
//! Example on how to declare an Input Built-in:
//! ```no_run
//! # use spirv_std_macros::gpu_only;
//! #
//! /// GLSL docs short description in #Name section. Remove the first "Contains " since we're using getters instead
//! /// of globals, capitalize and add a dot to the end.
//! ///
//! /// GLSL docs full #Description section.
//! ///
//! /// We're using GLSL documentation of this built-in, which is usually more descriptive than the SPIR-V or WGSL docs.
//! /// Change all references to link with rust-gpu intrinsics.
//! ///
//! /// Update the links of GLSL and WGSL to reference the correct page, keep SPIR-V as is. GLSL may link to the
//! /// [reference](https://registry.khronos.org/OpenGL-Refpages/gl4/) or to the
//! /// [glsl extensions github repo](https://github.com/KhronosGroup/GLSL/tree/main/extensions).
//! /// * GLSL: [`gl_MyBuiltIn`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_LocalInvocationID.xhtml)
//! /// * WGSL: [`my_built_in`](https://www.w3.org/TR/WGSL/#local-invocation-id-builtin-value)
//! /// * SPIRV: [`MyBuiltIn`](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
//! #[doc(alias = "gl_MyBuiltIn")]
//! #[doc(alias = "MyBuiltIn")]
//! #[inline]
//! #[gpu_only]
//! pub fn my_built_in() -> u32 {
//!     crate::load_builtin!(MyBuiltIn)
//! }
//! ```
//!
//! Reference links:
//! * [WGSL specification describing builtins](https://www.w3.org/TR/WGSL/#builtin-inputs-outputs)
//! * [SPIR-V specification for builtins](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_builtin)
//! * [GLSL reference](https://registry.khronos.org/OpenGL-Refpages/gl4/)
//!   * [GLSL reference source code](https://github.com/KhronosGroup/OpenGL-Refpages/tree/main/gl4)
//!   * [GLSL extensions](https://github.com/KhronosGroup/GLSL/tree/main/extensions)

/// Query SPIR-V (read-only global) built-in values
///
/// See [module level documentation] on how to use these.
#[macro_export]
macro_rules! load_builtin {
    ($name:ident $(: $ty:ty)? $(;default: $default:expr)? $(; extra: $($extra:expr),*)?) => {
        unsafe {
            let mut result $(: $ty)? = crate::load_builtin!(@default $($default)?);
            ::core::arch::asm! {
                "%builtin = OpVariable typeof{result_ref} Input",
                concat!("OpDecorate %builtin BuiltIn ", stringify!($name)),
                $($($extra,)*)?
                "%result = OpLoad typeof*{result_ref} %builtin",
                "OpStore {result_ref} %result",
                result_ref = in(reg) &mut result,
            }
            result
        }
    };
    (@default $default:expr) => {
        $default
    };
    (@default) => {
        Default::default()
    };
}

/// Declare an Output built-in, returning a `&'static mut T` to write the resulting value into.
#[macro_export]
macro_rules! decl_builtin_output {
    ($name:ident $(: $ty:ty)?) => {
        unsafe {
            let mut slot = ::core::mem::MaybeUninit$(::<&mut $ty>)?::uninit();
            ::core::arch::asm!(
                "%var = OpVariable typeof*{slot} Output",
                concat!("OpDecorate %var BuiltIn ", stringify!($name)),
                "OpStore {slot} %var",
                slot = in(reg) slot.as_mut_ptr(),
            );
            slot.assume_init()
        }
    };
}
