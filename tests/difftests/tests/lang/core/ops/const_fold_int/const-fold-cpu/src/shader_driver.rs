use crate::{EvalResult, INTERESTING_PATTERNS, Variants};
use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTest};

pub fn run(variant: Variants) {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    let test = WgpuComputeTest::new(
        RustComputeShader::default(),
        [64, 1, 1],
        Vec::from(&[
            BufferConfig::read_only(&[u32::from(variant)]),
            BufferConfig::read_only(&INTERESTING_PATTERNS),
            BufferConfig::writeback(size_of::<EvalResult>()),
        ]),
    );

    test.run_test(&config).unwrap();
}
