// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$SPIRV_STD_SRC/"
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"
// compile-flags: -C target-feature=+ImageQuery,+SampledRect
// ignore-vulkan1.0
// ignore-vulkan1.1
// ignore-vulkan1.1spv1.4
// ignore-vulkan1.2
// ignore-vulkan1.3
// ignore-vulkan1.4

use spirv_std::{Image, arch, image::SampledImage, spirv};

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
