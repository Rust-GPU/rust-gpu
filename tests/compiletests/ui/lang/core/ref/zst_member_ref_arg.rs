// build-pass

use spirv_std::spirv;
struct A;
struct B;

pub struct S<Z, W = ()> {
    x: A,
    y: B,

    z: Z,
    w: W,
}

fn f(x: &B) {}

#[spirv(fragment)]
pub fn main_zst_local() {
    let s = S { x: A, y: B, z: (), w: () };
    f(&s.y);
}

#[spirv(fragment)]
pub fn main_scalar(#[spirv(push_constant)] s: &S<usize>) {
    f(&s.y);
}

#[spirv(fragment)]
pub fn main_scalar_pair(#[spirv(push_constant)] s: &S<usize, usize>) {
    f(&s.y);
}

#[spirv(fragment)]
pub fn main_scalar_scalar_pair_nested(#[spirv(push_constant)] s: &S<(usize, usize)>) {
    f(&s.y);
}
