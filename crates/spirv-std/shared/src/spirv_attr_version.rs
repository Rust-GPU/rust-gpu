/// Returns the `spirv` attribute name with version tag.
///
/// The `#[spirv()]` attribute in `spirv_std` expands to this spirv attribute name,
/// including the version of `spirv_std`. `rustc_codegen_spirv` verifies that the
/// version matches with the codegen backend, to prevent accidental mismatches.
///
/// ```no_run
/// # use spirv_std_types::spirv_attr_version::spirv_attr_with_version;
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
