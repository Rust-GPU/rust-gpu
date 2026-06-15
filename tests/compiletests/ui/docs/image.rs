// built-pass

use spirv_std::Sampler;
use spirv_std::glam::*;
use spirv_std::image::*;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image2d: &Image2dI,
    #[spirv(descriptor_set = 1, binding = 1)] image2d_array: &Image2dArray,
    #[spirv(descriptor_set = 2, binding = 2)] cubemap: &Cubemap,
    #[spirv(descriptor_set = 3, binding = 3)] sampler: &Sampler,
) {
    let v2 = Vec2::new(0.0, 1.0);
    let v3 = Vec3::new(0.0, 1.0, 2.0);
    let r1: IVec4 = image2d.sample(*sampler, v2);
    let r2: Vec4 = image2d_array.sample(*sampler, v3);
    let r3: Vec4 = cubemap.sample(*sampler, v3);
}
