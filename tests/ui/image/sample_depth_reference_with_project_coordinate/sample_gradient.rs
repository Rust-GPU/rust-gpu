// Test `OpImageSampleProjDrefExplicitLod`
// build-pass

use spirv_std::{Image, Sampler};

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled),
    #[rust_gpu::spirv(descriptor_set = 1, binding = 1)] sampler: &Sampler,
    output: &mut f32,
) {
    let v2_dx = glam::Vec2::new(0.0, 1.0);
    let v2_dy = glam::Vec2::new(0.0, 1.0);
    let v3 = glam::Vec3A::new(0.0, 0.0, 1.0);
    *output = image.sample_depth_reference_with_project_coordinate_by_gradient(
        *sampler, v3, 1.0, v2_dx, v2_dy,
    );
}
