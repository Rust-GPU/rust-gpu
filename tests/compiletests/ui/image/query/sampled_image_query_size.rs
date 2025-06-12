// build-pass
// compile-flags: -C target-feature=+ImageQuery,+Sampled1D

use spirv_std::spirv;
use spirv_std::{Image, arch, image::SampledImage};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] sampled_image1d: &SampledImage<
        Image!(1D, type=f32, sampled),
    >,
    #[spirv(descriptor_set = 1, binding = 1)] sampled_image1d_array: &SampledImage<
        Image!(1D, type=f32, arrayed, sampled),
    >,
    #[spirv(descriptor_set = 2, binding = 2)] sampled_image2d: &SampledImage<
        Image!(2D, type=f32, sampled),
    >,
    #[spirv(descriptor_set = 3, binding = 3)] sampled_image2d_array: &SampledImage<
        Image!(2D, type=f32, arrayed, sampled),
    >,
    #[spirv(descriptor_set = 4, binding = 4)] sampled_image3d: &SampledImage<
        Image!(3D, type=f32, sampled),
    >,
    #[spirv(descriptor_set = 5, binding = 5)] sampled_image3d_array: &SampledImage<
        Image!(3D, type=f32, arrayed, sampled),
    >,
    output: &mut glam::UVec4,
) {
    // 1D images return scalar
    let size_1d: u32 = sampled_image1d.query_size_lod(0);

    // 1D arrayed images return 2 components (width, array_layers)
    let size_1d_array: glam::UVec2 = sampled_image1d_array.query_size_lod(0);

    // 2D images return 2 components
    let size_2d: glam::UVec2 = sampled_image2d.query_size_lod(0);

    // 2D arrayed images return 3 components
    let size_2d_array: glam::UVec3 = sampled_image2d_array.query_size_lod(0);

    // 3D images return 3 components
    let size_3d: glam::UVec3 = sampled_image3d.query_size_lod(0);

    // 3D arrayed images return 4 components (width, height, depth, array_layers)
    let size_3d_array: glam::UVec4 = sampled_image3d_array.query_size_lod(0);

    *output = glam::UVec4::new(
        size_1d,
        size_1d_array.y,
        size_2d.x + size_3d.z,
        size_3d_array.w,
    );
}
