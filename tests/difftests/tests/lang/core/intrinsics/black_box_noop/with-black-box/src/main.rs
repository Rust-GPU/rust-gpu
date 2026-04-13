use difftest::config::{Config, TestMetadata};
use difftest::scaffold::compute::{RustComputeShader, WgpuComputeTestMultiBuffer};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let test = WgpuComputeTestMultiBuffer::new_single_buffer(
        RustComputeShader::default(),
        [1, 1, 1],
        12 * 4,
    );
    test.run_test(&config).unwrap();
    config
        .write_metadata(&TestMetadata::u32())
        .expect("Failed to write metadata");
}
