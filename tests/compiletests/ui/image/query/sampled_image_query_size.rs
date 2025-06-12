// Test `OpImageQuerySizeLod` on `SampledImage`
// build-pass
// compile-flags: -C target-feature=+ImageQuery

use spirv_std::spirv;
use spirv_std::{Image, arch, image::SampledImage};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] sampled_image2d: &SampledImage<
        Image!(2D, type=f32, sampled),
    >,
    #[spirv(descriptor_set = 1, binding = 1)] sampled_image2d_array: &SampledImage<
        Image!(2D, type=f32, arrayed, sampled),
    >,
    #[spirv(descriptor_set = 2, binding = 2)] sampled_image3d: &SampledImage<
        Image!(3D, type=f32, sampled),
    >,
    output: &mut glam::UVec3,
) {
    // For sampled images, we need to use query_size_lod
    let size_2d: glam::UVec2 = sampled_image2d.query_size_lod(0);
    let size_2d_array: glam::UVec3 = sampled_image2d_array.query_size_lod(0);
    let size_3d: glam::UVec3 = sampled_image3d.query_size_lod(0);

    *output = glam::UVec3::new(size_2d.x, size_2d_array.y, size_3d.z);
}
