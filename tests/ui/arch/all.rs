// build-pass

#![feature(repr_simd)]

use core::num::NonZeroUsize;
use spirv_std::spirv;
use spirv_std::{scalar::Scalar, vector::Vector, vector::VectorOrScalar};

/// HACK(shesp). Rust doesn't allow us to declare regular (tuple-)structs containing `bool` members
/// as `#[repl(simd)]`. But we need this for `spirv_std::arch::any()` and `spirv_std::arch::all()`
/// to work.
/// Fortunately, this requirement isn't checked on generic structs, so we have a way to work around
/// it (for now at least)
#[repr(simd)]
#[derive(Copy, Clone, Debug)]
struct Vec2<T>(T, T);
unsafe impl<T: Scalar> VectorOrScalar for Vec2<T> {
    type Scalar = T;
    const DIM: NonZeroUsize = match NonZeroUsize::new(2) {
        None => panic!(),
        Some(n) => n,
    };
}
unsafe impl<T: Scalar> Vector<T, 2> for Vec2<T> {}

impl<T: Scalar> Default for Vec2<T> {
    fn default() -> Vec2<T> {
        Vec2(T::default(), T::default())
    }
}

#[spirv(fragment)]
pub fn main() {
    let vector = Vec2(true, true);
    assert!(spirv_std::arch::all(vector));
}
