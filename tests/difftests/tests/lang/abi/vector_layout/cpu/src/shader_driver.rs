use crate::glam_features::GlamFeatures;
use crate::layout::{LAYOUT_LEN, LAYOUT_RANGE};
use difftest::config::Config;
use difftest::scaffold::compute::{
    BufferConfig, ComputeShaderTest, RustComputeShader, WgpuBackend,
};

pub fn run(glam_feature: GlamFeatures) -> anyhow::Result<()> {
    glam_feature.assert();
    let config = Config::new()?;
    assert_eq!(0, LAYOUT_RANGE.start);
    ComputeShaderTest::<WgpuBackend, _>::new(
        RustComputeShader::default(),
        [LAYOUT_RANGE.end as u32, 1, 1],
        Vec::from(&[BufferConfig::writeback(LAYOUT_LEN * size_of::<u32>())]),
    )?
    .run_test(&config)
}
