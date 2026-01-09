use crate::{EvalResult, INTERESTING_PATTERNS, Variants};
use difftest::config::Config;
use difftest::scaffold::compute::{
    BufferConfig, ComputeShaderTest, RustComputeShader, WgpuBackend,
};

pub fn run(variant: Variants) -> anyhow::Result<()> {
    let config = Config::new()?;

    ComputeShaderTest::<WgpuBackend, _>::new(
        RustComputeShader::default(),
        [64, 1, 1],
        Vec::from(&[
            BufferConfig::read_only(&[u32::from(variant)]),
            BufferConfig::read_only(&INTERESTING_PATTERNS),
            BufferConfig::writeback(size_of::<EvalResult>()),
        ]),
    )?
    .run_test(&config)
}
