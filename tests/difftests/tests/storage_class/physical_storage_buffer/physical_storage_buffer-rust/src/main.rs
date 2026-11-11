#[cfg(not(target_arch = "spirv"))]
fn main() {
    use difftest::config::{Config, TestMetadata};
    use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTest};

    const DATA: [u32; 8] = [3, 1, 4, 1, 5, 9, 2, 6];
    const TAIL_LEN: u32 = 4;
    const NULL: u32 = u32::MAX;
    /// `[next, value]` of each node: the list is 2 -> 0 -> 3 -> 1.
    const NODES: [[u32; 2]; 4] = [[3, 10], [NULL, 20], [0, 30], [1, 40]];
    const HEAD: u32 = 2;

    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // The output must be the only writable buffer, as `run_test` writes all of them.
    let buffers = vec![
        BufferConfig::read_only(&[HEAD, DATA.len() as u32, TAIL_LEN]),
        BufferConfig::read_only(&DATA),
        BufferConfig::read_only(&NODES),
        BufferConfig::writeback(32),
    ];
    let test = WgpuComputeTest::new(RustComputeShader::default(), [1, 1, 1], buffers);

    config.write_metadata(&TestMetadata::u32()).unwrap();
    test.run_test(&config).unwrap();
}

#[cfg(target_arch = "spirv")]
fn main() {}
