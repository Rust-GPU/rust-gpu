// build-pass
// compile-flags: -C target-feature=+ImageQuery,+SampledRect
// ignore-vulkan1.0
// ignore-vulkan1.1
// ignore-vulkan1.1spv1.4
// ignore-vulkan1.2
// ignore-vulkan1.3
// ignore-vulkan1.4
// ignore-naga

use spirv_std::spirv;
use spirv_std::{Image, arch};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] rect_storage: &Image!(rect, type=f32, sampled=false),
    #[spirv(descriptor_set = 1, binding = 1)] rect_storage_array: &Image!(rect, type=f32, sampled=false, arrayed),
    output: &mut glam::UVec3,
) {
    // Rect images only support query_size (not query_size_lod)
    let rect_size: glam::UVec2 = rect_storage.query_size();

    // Arrayed rect images return 3D size (width, height, array_layers)
    let rect_array_size: glam::UVec3 = rect_storage_array.query_size();

    *output = glam::UVec3::new(rect_size.x, rect_size.y, rect_array_size.z);
}
