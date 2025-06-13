use serde::Deserialize;
use std::{fs, path::Path};

#[derive(Debug, Deserialize)]
pub struct Config {
    pub output_path: std::path::PathBuf,
}

impl Config {
    pub fn from_path<P: AsRef<Path>>(path: P) -> anyhow::Result<Self> {
        let content = fs::read_to_string(path)?;
        let config = serde_json::from_str(&content)?;
        Ok(config)
    }
}
