use crate::config::Config;
use anyhow::Result;

/// Configuration for a GPU buffer
#[derive(Clone)]
pub struct BufferConfig {
    pub size: u64,
    pub usage: BufferUsage,
    pub initial_data: Option<Vec<u8>>,
}

/// Buffer usage type
#[derive(Clone, Copy, PartialEq)]
pub enum BufferUsage {
    Storage,
    StorageReadOnly,
    Uniform,
}

/// A generic trait for compute backends
pub trait ComputeBackend: Sized {
    /// Initialize the backend
    fn init() -> Result<Self>;

    /// Create and run a compute shader with multiple buffers
    fn run_compute(
        &self,
        spirv_bytes: &[u8],
        entry_point: &str,
        dispatch: [u32; 3],
        buffers: Vec<BufferConfig>,
    ) -> Result<Vec<Vec<u8>>>;
}

/// A compute test that can run on any backend
pub struct ComputeTest<B: ComputeBackend> {
    backend: B,
    spirv_bytes: Vec<u8>,
    entry_point: String,
    dispatch: [u32; 3],
    buffers: Vec<BufferConfig>,
}

impl<B: ComputeBackend> ComputeTest<B> {
    pub fn new(
        spirv_bytes: Vec<u8>,
        entry_point: String,
        dispatch: [u32; 3],
        buffers: Vec<BufferConfig>,
    ) -> Result<Self> {
        Ok(Self {
            backend: B::init()?,
            spirv_bytes,
            entry_point,
            dispatch,
            buffers,
        })
    }

    pub fn run(self) -> Result<Vec<Vec<u8>>> {
        self.backend.run_compute(
            &self.spirv_bytes,
            &self.entry_point,
            self.dispatch,
            self.buffers,
        )
    }

    pub fn run_test(self, config: &Config) -> Result<()> {
        let buffers = self.buffers.clone();
        let outputs = self.run()?;
        // Write the first storage buffer output to the file
        for (output, buffer_config) in outputs.iter().zip(&buffers) {
            if matches!(buffer_config.usage, BufferUsage::Storage) && !output.is_empty() {
                use std::fs::File;
                use std::io::Write;
                let mut f = File::create(&config.output_path)?;
                f.write_all(output)?;
                return Ok(());
            }
        }
        anyhow::bail!("No storage buffer output found")
    }
}
