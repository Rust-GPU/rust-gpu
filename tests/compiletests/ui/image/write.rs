// Test `OpImageWrite`
// build-pass
// compile-flags: -C target-feature=+StorageImageWriteWithoutFormat

use spirv_std::glam::*;
use spirv_std::spirv;
use spirv_std::{Image, arch};

#[spirv(fragment)]
pub fn main(
    texels: Vec2,
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled=false),
) {
    unsafe {
        image.write(UVec2::new(0, 1), Vec4::from((texels, 0., 0.)));
    }
}
