// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// compile-flags: -C target-feature=+ImageQuery

use spirv_std::{arch, Image, Sampler};

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] image: &Image!(rect, type=f32, sampled),
    #[rust_gpu::spirv(descriptor_set = 0, binding = 1)] sampler: &Sampler,
    output: &mut glam::Vec2,
) {
    *output = image.query_lod(*sampler, glam::Vec2::new(0.0, 1.0));
}