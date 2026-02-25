// build-pass
// compile-flags: -Ctarget-feature=+Int64,+RayTracingKHR,+ext:SPV_KHR_ray_tracing

use spirv_std::ray_tracing::AccelerationStructure;
use spirv_std::ray_tracing::ray_pipeline::trace_ray;
use spirv_std::spirv;

#[spirv(ray_generation)]
pub fn main(#[spirv(ray_payload)] payload: &mut glam::Vec3) {
    unsafe {
        let handle = AccelerationStructure::from_u64(0xffff_ffff);
        let handle2 = AccelerationStructure::from_vec(glam::UVec2::new(0, 0));

        trace_ray(
            handle,
            spirv_std::ray_tracing::RayFlags::NONE,
            0,
            0,
            0,
            0,
            glam::vec3(1.0, 2.0, 3.0),
            0.5,
            glam::vec3(3.0, 2.0, 1.0),
            1.0,
            payload,
        );

        trace_ray(
            handle2,
            spirv_std::ray_tracing::RayFlags::NONE,
            0,
            0,
            0,
            0,
            glam::vec3(1.0, 2.0, 3.0),
            0.5,
            glam::vec3(3.0, 2.0, 1.0),
            1.0,
            payload,
        );
    }
}
