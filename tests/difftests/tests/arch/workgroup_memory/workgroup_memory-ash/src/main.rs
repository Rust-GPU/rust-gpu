#[cfg(not(target_arch = "spirv"))]
fn main() -> anyhow::Result<()> {
    use difftest::config::Config;

    let config = Config::new()?;

    // Skip on macOS due to Vulkan/MoltenVK configuration issues
    #[cfg(target_os = "macos")]
    {
        use difftest::scaffold::Skip;

        let skip = Skip::new("Ash tests are skipped on macOS due to MoltenVK configuration issues");
        skip.run_test(&config).unwrap();
        return;
    }

    // Run the actual test on other platforms
    #[cfg(not(target_os = "macos"))]
    {
        use difftest::scaffold::compute::{
            AshBackend, BufferConfig, BufferUsage, ComputeShaderTest, RustComputeShader,
        };
        use difftest::spirv_builder::Capability;

        // Initialize input buffer with values to sum
        let input_data: Vec<u32> = (1..=64).collect();
        let input_bytes = bytemuck::cast_slice(&input_data).to_vec();

        let buffers = vec![
            BufferConfig {
                size: 256, // 64 u32 values
                usage: BufferUsage::StorageReadOnly,
                initial_data: Some(input_bytes),
            },
            BufferConfig {
                size: 4, // 1 u32 value for output
                usage: BufferUsage::Storage,
                initial_data: None,
            },
        ];

        // Write metadata for U32 comparison
        config.write_metadata(&difftest::config::TestMetadata::u32())?;

        let shader = RustComputeShader::with_target(".", "spirv-unknown-vulkan1.2")
            .with_capability(Capability::VulkanMemoryModel);
        ComputeShaderTest::<AshBackend, _>::new(
            shader,
            [1, 1, 1], // Single workgroup with 64 threads
            buffers,
        )?
        .run_test(&config)
    }
}

#[cfg(target_arch = "spirv")]
fn main() -> anyhow::Result<()> {}
