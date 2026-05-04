#[cfg(not(target_arch = "spirv"))]
fn main() {
    use checked_mul_shader::{INPUTS, OUTPUT_LEN};
    use difftest::config::Config;
    use difftest::scaffold::compute::{
        AshBackend, BufferConfig, BufferUsage, ComputeShaderTest, RustComputeShader,
    };

    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // Skip on macOS — Ash tests have known MoltenVK configuration issues there.
    #[cfg(target_os = "macos")]
    {
        use difftest::scaffold::Skip;
        Skip::new("Ash tests are skipped on macOS due to MoltenVK configuration issues")
            .run_test(&config)
            .unwrap();
        return;
    }

    #[cfg(not(target_os = "macos"))]
    {
        let input_bytes: Vec<u8> = bytemuck::cast_slice(&INPUTS).to_vec();
        let output_size = (OUTPUT_LEN * std::mem::size_of::<u32>()) as u64;

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

        // Use the Ash backend so naga doesn't reject `OpUMulExtended` /
        // `OpSMulExtended` on its way to a non-Vulkan backend.
        let shader = RustComputeShader::default();
        let test = ComputeShaderTest::<AshBackend, _>::new(shader, [1, 1, 1], buffers).unwrap();
        test.run_test(&config).unwrap();
    }
}

#[cfg(target_arch = "spirv")]
fn main() {}
