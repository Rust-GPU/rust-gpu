// Tests that the invariant attribute works
// build-pass

use spirv_std::glam::Vec4;
use spirv_std::spirv;

#[spirv(vertex)]
pub fn main(#[spirv(invariant)] output: &mut f32, #[spirv(position)] pos: &mut Vec4) {
    *pos = Vec4::ZERO;
}
