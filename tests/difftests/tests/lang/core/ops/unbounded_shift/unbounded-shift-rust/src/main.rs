#[cfg(not(target_arch = "spirv"))]
fn main() {
    use difftest::config::{Config, TestMetadata};
    use difftest::scaffold::compute::{
        AshBackend, BufferConfig, BufferUsage, ComputeShaderTest, RustComputeShader,
    };
    use difftest::spirv_builder::Capability;
    use unbounded_shift_rust::{NUM_CASES, WORDS_PER_CASE, test_data};

    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // Skip on macOS due to Vulkan/MoltenVK configuration issues
    #[cfg(target_os = "macos")]
    {
        use difftest::scaffold::Skip;

        let skip = Skip::new("Ash tests are skipped on macOS due to MoltenVK configuration issues");
        skip.run_test(&config).unwrap();
        return;
    }

    #[cfg(not(target_os = "macos"))]
    {
        let input_bytes: Vec<u8> = bytemuck::cast_slice(&test_data()).to_vec();
        let output_size = (NUM_CASES * WORDS_PER_CASE * std::mem::size_of::<u32>()) as u64;

        let buffers = vec![
            BufferConfig {
                size: input_bytes.len() as u64,
                usage: BufferUsage::StorageReadOnly,
                initial_data: Some(input_bytes),
            },
            BufferConfig {
                size: output_size,
                usage: BufferUsage::Storage,
                initial_data: None,
            },
        ];

        // Use Ash backend since wgpu/naga doesn't support Int8, Int16 and Int64
        let shader = RustComputeShader::default()
            .with_capability(Capability::Int8)
            .with_capability(Capability::Int16)
            .with_capability(Capability::Int64);
        let num_workgroups = NUM_CASES as u32;
        let test = ComputeShaderTest::<AshBackend, _>::new(shader, [num_workgroups, 1, 1], buffers)
            .unwrap();

        config
            .write_metadata(&TestMetadata::u32())
            .expect("Failed to write metadata");

        test.run_test(&config).unwrap();
    }
}

#[cfg(target_arch = "spirv")]
fn main() {}
