// build-pass
// compile-flags: -C target-feature=+Int64
// compile-flags: -C llvm-args=--disassemble-fn=unbounded_shifts::shifts

use spirv_std::spirv;

// `unbounded_shl`/`unbounded_shr` must lower to `OpSelect`s without any branches.
#[inline(never)]
fn shifts(a: u32, b: i32, c: u64, shift: u32) -> u64 {
    let a = a.unbounded_shl(shift) ^ a.unbounded_shr(shift);
    let b = b.unbounded_shr(shift);
    let c = c.unbounded_shl(shift);
    (a ^ b as u32) as u64 ^ c
}

#[spirv(fragment)]
pub fn main(
    #[spirv(flat)] a: u32,
    #[spirv(flat)] b: i32,
    #[spirv(flat)] c: u64,
    #[spirv(flat)] shift: u32,
    out: &mut u64,
) {
    *out = shifts(a, b, c, shift);
}
