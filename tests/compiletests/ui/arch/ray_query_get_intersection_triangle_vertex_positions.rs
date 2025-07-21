// build-pass
// compile-flags: -Ctarget-feature=+RayQueryKHR,+ext:SPV_KHR_ray_query
// compile-flags: -Ctarget-feature=+RayQueryPositionFetchKHR,+ext:SPV_KHR_ray_tracing_position_fetch

use glam::{Vec3, Vec4};
use spirv_std::ray_tracing::{AccelerationStructure, RayFlags, RayQuery};
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] accel: &AccelerationStructure,
    out0: &mut Vec4,
    out1: &mut Vec4,
    out2: &mut Vec4,
) {
    unsafe {
        spirv_std::ray_query!(let mut handle);
        handle.initialize(accel, RayFlags::NONE, 0, Vec3::ZERO, 0.0, Vec3::ZERO, 0.0);
        let vertex_positions: [Vec3; 3] = handle.get_intersection_triangle_vertex_positions();
        *out0 = Vec4::from((vertex_positions[0], 1.));
        *out1 = Vec4::from((vertex_positions[1], 1.));
        *out2 = Vec4::from((vertex_positions[2], 1.));
    }
}
