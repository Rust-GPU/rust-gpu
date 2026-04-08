//! Mainly for the Linkage struct, which is written to a json file.

/// Shader source and entry point that can be used to create shader linkage.
#[derive(serde::Serialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Linkage {
    /// File path to the entry point's source file
    pub source_path: String,
    /// Name of the entry point for spirv and vulkan
    pub entry_point: String,
    /// Name of the entry point for wgsl, where `::` characters have been removed
    pub wgsl_entry_point: String,
}

impl Linkage {
    /// Make a new `Linkage` from an entry point and source path
    #[expect(clippy::impl_trait_in_params, reason = "just a struct new")]
    pub fn new(entry_point: impl AsRef<str>, source_path: impl AsRef<std::path::Path>) -> Self {
        Self {
            // Force a forward slash convention here so it works on all OSs
            source_path: source_path
                .as_ref()
                .components()
                .map(|comp| comp.as_os_str().to_string_lossy())
                .collect::<Vec<_>>()
                .join("/"),
            wgsl_entry_point: entry_point.as_ref().replace("::", ""),
            entry_point: entry_point.as_ref().to_owned(),
        }
    }
}
