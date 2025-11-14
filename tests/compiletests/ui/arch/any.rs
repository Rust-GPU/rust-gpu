// build-pass

use core::num::NonZeroUsize;
use spirv_std::spirv;
use spirv_std::{scalar::Scalar, vector::Vector, vector::VectorOrScalar};

#[spirv(fragment)]
pub fn main() {
    let vector = glam::BVec2::new(false, true);
    assert!(spirv_std::arch::any(vector));
}
