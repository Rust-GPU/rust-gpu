use crate::common::{OUT_LEN, input_dataset};
use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTestMultiBuffer};

pub fn run() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let test = WgpuComputeTestMultiBuffer::new(
        RustComputeShader::default(),
        [1, 1, 1],
        Vec::from(&[
            BufferConfig::read_only(&input_dataset()),
            BufferConfig::writeback(OUT_LEN * size_of::<u32>()),
        ]),
    );
    test.run_test(&config).unwrap();
}
