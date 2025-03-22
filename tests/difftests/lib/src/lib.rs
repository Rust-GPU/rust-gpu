pub mod config;
pub mod scaffold;

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
