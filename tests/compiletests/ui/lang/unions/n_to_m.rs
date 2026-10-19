// build-pass
// compile-flags: -C target-feature=+Int64

use spirv_std::spirv;

union MyUnion {
    a: (u32, u64),
    b: (u64, u32),
}

#[spirv(fragment)]
pub fn union_mixed_types(
    out: &mut u64,
) {
    let bla = MyUnion { a: (42, 123) };
    *out = unsafe { bla.b }.0;
}
