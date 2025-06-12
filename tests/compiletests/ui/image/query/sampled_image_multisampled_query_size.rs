// build-pass
// compile-flags: -C target-feature=+ImageQuery

use spirv_std::spirv;
use spirv_std::{Image, arch, image::SampledImage};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] sampled_image2d_ms: &SampledImage<
        Image!(2D, type=f32, multisampled, sampled),
    >,
    output: &mut glam::UVec2,
) {
    // Multisampled sampled images can use query_size directly
    *output = sampled_image2d_ms.query_size();
}
