// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$SPIRV_STD_SRC/"
// compile-flags: -C target-feature=+ImageQuery,+SampledRect

use spirv_std::{Image, arch, spirv, image::SampledImage};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] rect_sampled: &SampledImage<
        Image!(rect, type=f32, sampled),
    >,
    output: &mut glam::UVec2,
) {
    // This should fail because rect images don't support query_size_lod
    *output = rect_sampled.query_size_lod(0);
}