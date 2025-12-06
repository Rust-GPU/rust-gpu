// Test `OpImageWrite`
// build-pass

use spirv_std::glam::*;
use spirv_std::spirv;
use spirv_std::{Image, arch};

#[spirv(fragment)]
pub fn main(
    texels: f32,
    #[spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, format = r32f, sampled = false),
) {
    unsafe {
        image.write(UVec2::new(0, 1), texels);
    }
}
