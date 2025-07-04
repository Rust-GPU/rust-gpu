#![cfg_attr(target_arch = "spirv", no_std)]

#[cfg(not(target_arch = "spirv"))]
pub mod config;
#[cfg(not(target_arch = "spirv"))]
pub mod scaffold;

/// Macro to round a f32 value to 6 decimal places for cross-platform consistency
/// in floating-point operations. This helps ensure difftest results are consistent
/// across different platforms (Linux, Mac, Windows) which may have slight differences
/// in floating-point implementations.
#[macro_export]
macro_rules! round6 {
    ($v:expr) => {
        (($v) * 1_000_000.0).round() / 1_000_000.0
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
        let config_json = r#"{ "output_path": "/tmp/output.txt" }"#;
        write!(tmp, "{}", config_json).unwrap();
        let config = Config::from_path(tmp.path()).unwrap();
        assert_eq!(config.output_path.to_str().unwrap(), "/tmp/output.txt");
    }
}
