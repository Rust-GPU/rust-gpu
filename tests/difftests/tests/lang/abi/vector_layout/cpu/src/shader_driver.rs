use crate::glam_features::GlamFeatures;
use crate::layout::{LAYOUT_LEN, LAYOUT_RANGE};
use difftest::config::Config;
use difftest::scaffold::compute::{BufferConfig, RustComputeShader, WgpuComputeTestMultiBuffer};

pub fn run(glam_feature: GlamFeatures) {
    glam_feature.assert();
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    assert_eq!(0, LAYOUT_RANGE.start);
    let test = WgpuComputeTestMultiBuffer::new(
        RustComputeShader::default(),
        [LAYOUT_RANGE.end as u32, 1, 1],
        Vec::from(&[BufferConfig::writeback(LAYOUT_LEN * size_of::<u32>())]),
    );
    test.run_test(&config).unwrap();
}
