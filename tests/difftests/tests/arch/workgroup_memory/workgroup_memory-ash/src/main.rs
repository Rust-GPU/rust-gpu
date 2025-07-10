#[cfg(not(target_arch = "spirv"))]
fn main() {
    use difftest::config::Config;

    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

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
        use difftest::scaffold::compute::{AshBackend, BufferConfig, BufferUsage, ComputeTest};
        use difftest::spirv_builder::{
            Capability, MetadataPrintout, ModuleResult, ShaderPanicStrategy, SpirvBuilder,
        };
        use std::fs;

        // Build the Rust shader to SPIR-V
        let builder = SpirvBuilder::new(".", "spirv-unknown-vulkan1.2")
            .print_metadata(MetadataPrintout::None)
            .release(true)
            .multimodule(false)
            .shader_panic_strategy(ShaderPanicStrategy::SilentExit)
            .preserve_bindings(true)
            .capability(Capability::VulkanMemoryModel);

        let artifact = builder.build().expect("Failed to build SPIR-V");

        if artifact.entry_points.len() != 1 {
            panic!(
                "Expected exactly one entry point, found {}",
                artifact.entry_points.len()
            );
        }
        let entry_point = artifact.entry_points.into_iter().next().unwrap();

        let spirv_bytes = match artifact.module {
            ModuleResult::SingleModule(path) => {
                fs::read(&path).expect("Failed to read SPIR-V file")
            }
            ModuleResult::MultiModule(_) => panic!("Unexpected multi-module result"),
        };

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

        let test = ComputeTest::<AshBackend>::new(
            spirv_bytes,
            entry_point,
            [1, 1, 1], // Single workgroup with 64 threads
            buffers,
        )
        .unwrap();

        // Write metadata for U32 comparison
        let metadata = difftest::config::TestMetadata {
            epsilon: None,
            output_type: difftest::config::OutputType::U32,
            ..Default::default()
        };
        config.write_metadata(&metadata).unwrap();

        test.run_test(&config).unwrap();
    }
}

#[cfg(target_arch = "spirv")]
fn main() {}
