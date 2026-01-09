#![cfg_attr(target_arch = "spirv", no_std)]

#[cfg(not(target_arch = "spirv"))]
pub use difftest_types::config;
#[cfg(not(target_arch = "spirv"))]
pub mod scaffold;

#[cfg(not(target_arch = "spirv"))]
pub use anyhow;
#[cfg(not(target_arch = "spirv"))]
pub use ash;
#[cfg(not(target_arch = "spirv"))]
pub use spirv_builder;
#[cfg(not(target_arch = "spirv"))]
pub use wgpu;

/// Macro to round a f32 value for cross-platform compatibility in floating-point
/// operations. This helps ensure difftest results are consistent across different
/// platforms (Linux, Mac, Windows) which may have slight differences in floating-point
/// implementations due to different FMA usage, operation ordering, etc.
///
/// We round to 5 decimal places to handle differences that can appear in the 6th-7th
/// decimal places due to platform variations.
#[macro_export]
macro_rules! compat_round {
    ($v:expr) => {
        (($v) * 100_000.0).round() / 100_000.0
    };
}

#[cfg(test)]
mod tests {
    use super::config::Config;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_config_from_path() {
        let mut tmp = NamedTempFile::new().unwrap();
        let config_json =
            r#"{ "output_path": "/tmp/output.txt", "metadata_path": "/tmp/metadata.json" }"#;
        write!(tmp, "{config_json}").unwrap();
        let config = Config::from_path(tmp.path()).unwrap();
        assert_eq!(config.output_path.to_str().unwrap(), "/tmp/output.txt");
        assert_eq!(config.metadata_path.to_str().unwrap(), "/tmp/metadata.json");
    }
}
