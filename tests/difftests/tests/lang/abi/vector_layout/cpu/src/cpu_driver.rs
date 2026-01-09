use crate::glam_features::GlamFeatures;
use crate::layout::{LAYOUT_LEN, LAYOUT_RANGE, eval_layouts};
use difftest::config::Config;

pub fn run(glam_feature: GlamFeatures) {
    glam_feature.assert();
    let config = Config::new()?;
    let mut out = vec![0; LAYOUT_LEN];
    for gid in LAYOUT_RANGE {
        eval_layouts(gid as u32, &mut out);
    }
    config.write_result(&out).unwrap()
}
