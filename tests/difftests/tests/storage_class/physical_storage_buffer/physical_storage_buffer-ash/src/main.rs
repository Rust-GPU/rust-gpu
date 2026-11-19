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
        use difftest::config::TestMetadata;
        use difftest::scaffold::Skip;
        use difftest::scaffold::compute::{
            AshBackend, BufferConfig, BufferUsage, RustComputeShader, SpirvShader,
        };
        use difftest::spirv_builder::Capability;

        const DATA: [u32; 8] = [3, 1, 4, 1, 5, 9, 2, 6];
        const TAIL_LEN: u32 = 4;
        /// `(next, value)` of each node: the list is 2 -> 0 -> 3 -> 1.
        const NODES: [(Option<u64>, u32); 4] =
            [(Some(3), 10), (None, 20), (Some(0), 30), (Some(1), 40)];
        const HEAD: u64 = 2;
        const NODE_SIZE: u64 = 16;
        /// `Output`, then `TAIL_LEN` words.
        const OUTPUT_SIZE: u64 = 32;

        let Some(backend) = AshBackend::init_with_buffer_device_address().unwrap() else {
            let skip = Skip::new("The Vulkan device lacks `bufferDeviceAddress` or `shaderInt64`");
            skip.run_test(&config).unwrap();
            return;
        };

        let shader = RustComputeShader::with_target(".", "spirv-unknown-vulkan1.2")
            .with_capability(Capability::PhysicalStorageBufferAddresses)
            .with_capability(Capability::Int64);
        let (spirv_bytes, entry_point) = shader.spirv_bytes().unwrap();

        let buffers = vec![
            // `Roots`, written once the addresses are known
            BufferConfig {
                size: 32,
                usage: BufferUsage::StorageReadOnly,
                initial_data: None,
            },
            BufferConfig::read_only(&DATA),
            BufferConfig {
                size: NODES.len() as u64 * NODE_SIZE,
                usage: BufferUsage::StorageReadOnly,
                initial_data: None,
            },
            BufferConfig {
                size: OUTPUT_SIZE,
                usage: BufferUsage::Storage,
                // Unlike wgpu, ash doesn't zero new buffers.
                initial_data: Some(vec![0; OUTPUT_SIZE as usize]),
            },
        ];
        let outputs = backend
            .run_compute_with_addresses(&spirv_bytes, &entry_point, [1, 1, 1], buffers, |addrs| {
                let (data, nodes, output) = (addrs[1], addrs[2], addrs[3]);

                let mut roots = Vec::new();
                for ptr in [data, nodes + HEAD * NODE_SIZE, output] {
                    roots.extend(ptr.to_le_bytes());
                }
                for count in [DATA.len() as u32, TAIL_LEN] {
                    roots.extend(count.to_le_bytes());
                }

                let mut node_bytes = Vec::new();
                for (next, value) in NODES {
                    let next = next.map_or(0, |i| nodes + i * NODE_SIZE);
                    node_bytes.extend(next.to_le_bytes());
                    node_bytes.extend(value.to_le_bytes());
                    node_bytes.extend(0u32.to_le_bytes());
                }

                vec![Some(roots), None, Some(node_bytes), None]
            })
            .unwrap();

        config.write_metadata(&TestMetadata::u32()).unwrap();
        // Only the output buffer, which holds no addresses.
        config.write_result(&outputs[3]).unwrap();
    }
}

#[cfg(target_arch = "spirv")]
fn main() {}
