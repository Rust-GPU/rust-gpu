use difftest::config::Config;
use difftest::scaffold::compute::{ComputeShaderTest, RustComputeShader, WgpuBackend};

fn main() -> anyhow::Result<()> {
    let config = Config::new()?;

    let buffer_size = 1024;
    ComputeShaderTest::<WgpuBackend, _>::new_with_sizes(
        RustComputeShader::default(),
        [64, 1, 1],
        &[buffer_size, buffer_size],
    )?
    .run_test(&config)
}
