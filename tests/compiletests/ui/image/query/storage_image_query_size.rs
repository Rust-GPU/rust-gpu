// build-pass
// compile-flags: -C target-feature=+ImageQuery,+Sampled1D,+SampledBuffer

use spirv_std::spirv;
use spirv_std::{Image, arch};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] buffer_image: &Image!(buffer, type=f32, sampled=false),
    #[spirv(descriptor_set = 1, binding = 1)] storage_1d: &Image!(1D, type=f32, sampled=false),
    #[spirv(descriptor_set = 2, binding = 2)] storage_1d_array: &Image!(1D, type=f32, sampled=false, arrayed),
    #[spirv(descriptor_set = 3, binding = 3)] storage_3d_array: &Image!(3D, type=f32, sampled=false, arrayed),
    output: &mut glam::UVec4,
) {
    // Buffer images return scalar (number of texels)
    let buffer_size: u32 = buffer_image.query_size();

    // 1D storage images return scalar
    let size_1d: u32 = storage_1d.query_size();

    // 1D arrayed storage images return 2 components
    let size_1d_array: glam::UVec2 = storage_1d_array.query_size();

    // 3D arrayed storage images return 4 components
    let size_3d_array: glam::UVec4 = storage_3d_array.query_size();

    *output = glam::UVec4::new(buffer_size, size_1d, size_1d_array.y, size_3d_array.w);
}
