#[cfg(not(target_arch = "spirv"))]
fn main() {
    use difftest::config::{Config, TestMetadata};
    use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTest};
    use fill_offset_zero_rust::{INPUT, Output};

    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let test = WgpuComputeTest::new(
        RustComputeShader::default(),
        [1, 1, 1],
        vec![
            BufferConfig::read_only(&INPUT),
            BufferConfig::writeback(size_of::<Output>()),
        ],
    );
    config.write_metadata(&TestMetadata::u32()).unwrap();
    test.run_test(&config).unwrap();
}

#[cfg(target_arch = "spirv")]
fn main() {}
