use crate::layout::{LAYOUT_COUNT, LAYOUT_LEN};
use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTestMultiBuffer};

pub fn run() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let test = WgpuComputeTestMultiBuffer::new(
        RustComputeShader::default(),
        [LAYOUT_COUNT as u32, 1, 1],
        Vec::from(&[BufferConfig::writeback(LAYOUT_LEN * size_of::<u32>())]),
    );
    test.run_test(&config).unwrap();
}
