// build-pass

use spirv_std::spirv;
use spirv_std::{Image, Sampler, arch, image::ImageWithMethods, image::sample_with};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] sampler: &Sampler,
    #[spirv(descriptor_set = 0, binding = 1)] image1: &Image!(2D, type=f32, sampled, multisampled),
    #[spirv(descriptor_set = 0, binding = 2)] image2: &Image!(2D, type=f32, sampled),
    output: &mut glam::Vec4,
) {
    let t1 = image1.fetch_with(glam::IVec2::new(0, 0), sample_with::sample_index(1));
    // FIXME(eddyb) get `f32` to be automatically inferred instead of the `f64` default.
    let t2 = image2.sample_with(
        *sampler,
        glam::Vec2::new(0.5, 0.5),
        sample_with::bias(1.0f32),
    );
    let t3 = image2.sample_with(
        *sampler,
        glam::Vec2::new(0.5, 0.5),
        sample_with::lod(2.0f32),
    );
    let t4 = image2.sample_with(
        *sampler,
        glam::Vec2::new(0.5, 0.5),
        sample_with::grad(glam::Vec2::new(0.5, 0.5), glam::Vec2::new(0.5, 0.5)),
    );
    *output = t1 + t2 + t3 + t4;
}
