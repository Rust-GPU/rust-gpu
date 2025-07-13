// build-pass

use spirv_std::glam::UVec3;
use spirv_std::spirv;

#[spirv(compute(threads(8, 4, 2)))]
pub fn main(#[spirv(workgroup_size)] size: UVec3, #[spirv(local_invocation_id)] local_id: UVec3) {
    // The workgroup_size should be (8, 4, 2)
    assert!(size.x == 8);
    assert!(size.y == 4);
    assert!(size.z == 2);
}
