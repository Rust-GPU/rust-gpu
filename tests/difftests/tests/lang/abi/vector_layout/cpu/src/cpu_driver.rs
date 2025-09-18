use crate::glam_features::GlamFeatures;
use crate::layout::{LAYOUT_COUNT, LAYOUT_LEN, eval_layouts};
use difftest::config::Config;

pub fn run(glam_feature: GlamFeatures) {
    glam_feature.assert();
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let mut out = vec![0; LAYOUT_LEN];
    for gid in 0..LAYOUT_COUNT {
        eval_layouts(gid as u32, &mut out);
    }
    config.write_result(&out).unwrap()
}
