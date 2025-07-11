// build-pass

use spirv_std::spirv;

#[derive(Copy, Clone, Default)]
struct Foo {
    bar: bool,
    baz: [[u32; 2]; 1],
}

#[spirv(fragment)]
pub fn main() {
    let x = [[1; 2]; 1];
    let y = [Foo::default(); 1];
}

// HACK(eddyb) future-proofing against `[expr; 1]` -> `[expr]`
// MIR optimization (https://github.com/rust-lang/rust/pull/135322).
fn force_repeat_one<const ONE: usize>() -> ([[u32; 2]; ONE], [Foo; ONE]) {
    ([[1; 2]; ONE], [Foo::default(); ONE])
}

#[spirv(fragment)]
pub fn main_future_proof(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] out: &mut [[u32; 2]; 2],
) {
    let (x, y) = force_repeat_one::<1>();

    // NOTE(eddyb) further guard against optimizations by using `x` and `y`.
    out[0] = x[0];
    out[1] = y[0].baz[0];
}
