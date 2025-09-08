use difftest::config::{Config, TestMetadata};
use difftest::scaffold::compute::{
    BufferConfig, BufferUsage, RustComputeShader, WgpuComputeTestPushConstants,
};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    // One storage buffer (output), plus push constants data (pair of u32)
    let sizes = [21 * 4u64];
    let push_data: Vec<u8> = {
        let a: u32 = 100;
        let b: u32 = 200;
        let mut v = Vec::with_capacity(8);
        v.extend_from_slice(&a.to_le_bytes());
        v.extend_from_slice(&b.to_le_bytes());
        v
    };
    let test = WgpuComputeTestPushConstants::new(
        RustComputeShader::default(),
        [1, 1, 1],
        vec![BufferConfig {
            size: sizes[0],
            usage: BufferUsage::Storage,
            initial_data: None,
        }],
        8,
        push_data,
    );
    test.run_test(&config).unwrap();
    config
        .write_metadata(&TestMetadata::u32())
        .expect("Failed to write metadata");
}
