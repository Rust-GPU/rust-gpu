// build-pass

use spirv_std::spirv;

struct S {
    x: u32,
    y: u32,
}

// NOTE(eddyb) `#[inline(never)]` is for blocking inlining at the e.g. MIR level,
// whereas any Rust-GPU-specific legalization will intentionally ignore it.

#[inline(never)]
fn f(x: &u32) -> u32 {
    *x
}

#[inline(never)]
fn g(xy: (&u32, &u32)) -> (u32, u32) {
    (*xy.0, *xy.1)
}

#[inline(never)]
fn h(xyz: (&u32, &u32, &u32)) -> (u32, u32, u32) {
    (*xyz.0, *xyz.1, *xyz.2)
}

#[inline(never)]
fn h_newtyped(xyz: ((&u32, &u32, &u32),)) -> (u32, u32, u32) {
    (*xyz.0.0, *xyz.0.1, *xyz.0.2)
}

#[spirv(fragment)]
pub fn main() {
    let s = S { x: 2, y: 2 };
    f(&s.x);
    g((&s.x, &s.y));
    h((&s.x, &s.y, &s.x));
    h_newtyped(((&s.x, &s.y, &s.x),));
}
