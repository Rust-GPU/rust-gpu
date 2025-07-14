use crate::config::Config;
use crate::scaffold::shader::SpirvShader;
use anyhow::Result;

/// Configuration for a GPU buffer
#[derive(Clone)]
pub struct BufferConfig {
    pub size: u64,
    pub usage: BufferUsage,
    pub initial_data: Option<Vec<u8>>,
}

impl BufferConfig {
    pub fn writeback(size: usize) -> Self {
        Self {
            size: size as u64,
            usage: BufferUsage::Storage,
            initial_data: None,
        }
    }

    pub fn read_only<A: bytemuck::NoUninit>(slice: &[A]) -> Self {
        let vec = bytemuck::cast_slice(slice).to_vec();
        Self {
            size: vec.len() as u64,
            usage: BufferUsage::StorageReadOnly,
            initial_data: Some(vec),
        }
    }
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

    /// Create and run a compute shader with multiple buffers from raw SPIRV bytes
    fn run_compute(
        &self,
        spirv_bytes: &[u8],
        entry_point: &str,
        dispatch: [u32; 3],
        buffers: Vec<BufferConfig>,
    ) -> Result<Vec<Vec<u8>>>;

    /// Create and run a compute shader with multiple buffers from a shader object
    fn run_compute_shader<S: SpirvShader>(
        &self,
        shader: &S,
        dispatch: [u32; 3],
        buffers: Vec<BufferConfig>,
    ) -> Result<Vec<Vec<u8>>> {
        let (spirv_bytes, entry_point) = shader.spirv_bytes()?;
        self.run_compute(&spirv_bytes, &entry_point, dispatch, buffers)
    }
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
                config.write_result(output)?;
                return Ok(());
            }
        }
        anyhow::bail!("No storage buffer output found")
    }
}

/// A compute test that can run on any backend using a shader object
pub struct ComputeShaderTest<B: ComputeBackend, S: SpirvShader> {
    backend: B,
    shader: S,
    dispatch: [u32; 3],
    buffers: Vec<BufferConfig>,
}

impl<B: ComputeBackend, S: SpirvShader> ComputeShaderTest<B, S> {
    pub fn new(shader: S, dispatch: [u32; 3], buffers: Vec<BufferConfig>) -> Result<Self> {
        Ok(Self {
            backend: B::init()?,
            shader,
            dispatch,
            buffers,
        })
    }

    pub fn run(self) -> Result<Vec<Vec<u8>>> {
        self.backend
            .run_compute_shader(&self.shader, self.dispatch, self.buffers)
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
