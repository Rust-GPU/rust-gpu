use serde::{Deserialize, Serialize};
use std::{fs, path::Path};

#[derive(Debug, Deserialize, Serialize)]
pub struct Config {
    pub output_path: std::path::PathBuf,
    pub metadata_path: std::path::PathBuf,
}

/// Test metadata that controls output comparison behavior
///
/// This metadata is written alongside test output to specify how the test harness
/// should compare outputs between different implementations (e.g., Rust vs WGSL).
#[derive(Debug, Serialize, Deserialize, Default)]
pub struct TestMetadata {
    /// Maximum allowed difference for floating-point comparisons.
    ///
    /// When None (default), exact byte-for-byte comparison is used.
    /// For floating-point tests, a small epsilon (e.g., 1e-6) accounts for
    /// platform-specific precision differences.
    #[serde(default)]
    pub epsilon: Option<f32>,

    /// Specifies how to interpret and compare output data.
    ///
    /// Defaults to `Raw` for exact byte comparison. Use typed variants
    /// (F32, F64, etc.) to enable epsilon-based comparison for numeric types.
    #[serde(default)]
    pub output_type: OutputType,

    /// If present, indicates this test was skipped with the given reason
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub skipped: Option<String>,
}

/// Specifies how test output data should be interpreted for comparison
#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum OutputType {
    /// Exact byte-for-byte comparison (default)
    #[default]
    Raw,
    /// Interpret as array of 32-bit floats, enables epsilon comparison
    F32,
    /// Interpret as array of 64-bit floats, enables epsilon comparison  
    F64,
    /// Interpret as array of 32-bit unsigned integers
    U32,
    /// Interpret as array of 32-bit signed integers
    I32,
}

impl TestMetadata {
    /// Create metadata with a specific epsilon value, keeping default output type
    pub fn with_epsilon(epsilon: f32) -> Self {
        Self {
            epsilon: Some(epsilon),
            ..Default::default()
        }
    }
}

impl Config {
    pub fn from_path<P: AsRef<Path>>(path: P) -> anyhow::Result<Self> {
        let content = fs::read_to_string(path)?;
        let config = serde_json::from_str(&content)?;
        Ok(config)
    }

    /// Write test metadata to the configured metadata path
    pub fn write_metadata(&self, metadata: &TestMetadata) -> anyhow::Result<()> {
        let metadata_json = serde_json::to_string(metadata)?;
        fs::write(&self.metadata_path, metadata_json)?;
        Ok(())
    }
}
