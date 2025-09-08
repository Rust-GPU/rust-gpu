use crate::layout::EvalResult;
use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTestMultiBuffer};

pub fn run() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let test = WgpuComputeTestMultiBuffer::new(
        RustComputeShader::default(),
        [1, 1, 1],
        Vec::from(&[BufferConfig::writeback(size_of::<EvalResult>())]),
    );
    test.run_test(&config).unwrap();
}
