// build-fail

use spirv_std::glam::UVec3;
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(#[spirv(local_invocation_index)] index: UVec3) {}
