// build-pass
// compile-flags: -C target-feature=+ImageQuery

use spirv_std::spirv;
use spirv_std::{Image, arch, image::Cubemap};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] cubemap: &Cubemap,
    #[spirv(descriptor_set = 1, binding = 1)] cubemap_array: &Image!(cube, type=f32, sampled, arrayed),
    #[spirv(descriptor_set = 2, binding = 2)] storage_cubemap: &Image!(cube, type=f32, sampled=false),
    output: &mut glam::UVec3,
) {
    // Cubemaps return 2D size (width, height of one face)
    let size: glam::UVec2 = cubemap.query_size_lod(0);

    // Arrayed cubemaps return 3D size (width, height, array_layers)
    let size_array: glam::UVec3 = cubemap_array.query_size_lod(0);

    // Storage cubemaps can use query_size directly
    let storage_size: glam::UVec2 = storage_cubemap.query_size();

    *output = glam::UVec3::new(size.x, size_array.z, storage_size.x);
}
