use difftest::config::Config;
use difftest::scaffold::compute::{
    BufferConfig, BufferUsage, RustComputeShader, WgpuComputeTestMultiBuffer,
};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    let buffer_size = 256;
    let initial_data: Vec<u32> = (0..64).collect();
    let initial_bytes: Vec<u8> = initial_data.iter().flat_map(|&x| x.to_ne_bytes()).collect();

    let test = WgpuComputeTestMultiBuffer::new(
        RustComputeShader::default(),
        [1, 1, 1],
        vec![
            BufferConfig {
                size: buffer_size,
                usage: BufferUsage::StorageReadOnly,
                initial_data: Some(initial_bytes),
            },
            BufferConfig {
                size: buffer_size,
                usage: BufferUsage::Storage,
                initial_data: None,
            },
        ],
    );

    test.run_test(&config).unwrap();
}
