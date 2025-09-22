//! This is placed outside any crate, and included by both `spirv_std` and `rustc_codegen_spirv`.
//! I could have made a new crate, shared between the two, but decided against having even more small crates for sharing
//! types. Instead, you get this single small file to specify the versioned spirv attribute.
//!
//! This also ensures that the macros below take the *exact* version of the two crates above, and not some dependency
//! that both of them depend on.

/// The spirv attribute with version tag
///
/// ```ignore
/// # we don't know the namespace of our function
/// let spirv = spirv_attr_with_version();
/// let attr = format!("#[rust_gpu::{spirv}(vertex)]");
/// // version here may be out-of-date
/// assert_eq!("#[rust_gpu::spirv_v0_9(vertex)]", attr);
/// ```
pub fn spirv_attr_with_version() -> String {
    let major: u32 = env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap();
    let minor: u32 = env!("CARGO_PKG_VERSION_MINOR").parse().unwrap();
    format!("spirv_v{major}_{minor}")
}
