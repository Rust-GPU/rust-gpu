use crate::config::{Config, TestMetadata};

/// A scaffolding type for tests that should be skipped on certain platforms
pub struct Skip {
    message: &'static str,
}

impl Skip {
    /// Create a Skip test with a reason message
    pub fn new(message: &'static str) -> Self {
        Self { message }
    }

    /// Run the skip test - writes metadata indicating the test should be skipped
    pub fn run_test(&self, config: &Config) -> anyhow::Result<()> {
        // Write metadata indicating this test was skipped
        let metadata = TestMetadata {
            skipped: Some(self.message.to_string()),
            ..Default::default()
        };
        config.write_metadata(&metadata)?;

        Ok(())
    }
}
