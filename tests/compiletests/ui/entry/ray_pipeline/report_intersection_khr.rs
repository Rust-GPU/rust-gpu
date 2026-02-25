// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+ext:SPV_KHR_ray_tracing

use spirv_std::spirv;

#[spirv(intersection)]
pub fn main() {
    unsafe {
        spirv_std::ray_tracing::ray_pipeline::report_intersection(2.0, 4);
    }
}
