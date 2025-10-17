// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=black_box::disassemble
#![no_std]

use core::hint::black_box;
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut [u32]) {
    let r = disassemble();
    for i in 0..r.len() {
        out[i] = r[i];
    }
}

#[inline(never)]
pub fn disassemble() -> [u32; 12] {
    let x = 42i32;
    // Immediate: integer scalar passes through unchanged
    let y = black_box(x);

    let a = 3.14f32;
    // Immediate: float scalar passes through unchanged
    let b = black_box(a);

    let v = [1u32, 2, 3, 4];
    // Ref: non-immediate aggregate is loaded from memory
    let w = black_box(v);

    // Immediate: constants are immediates
    let result = black_box(10) + black_box(20);

    let data = 100u32;
    // Immediate (pointer): reference value is an immediate scalar pointer
    let ref_data = black_box(&data);

    // Pair: two-element tuple packs into a single SSA aggregate
    let pair = (5u32, 6u32);
    let pair_bb = black_box(pair);
    let pair_sum = pair_bb.0 + pair_bb.1;

    // ZeroSized: unit type becomes `undef` of the right type
    let _z = black_box(());

    [
        y as u32,
        f32::to_bits(b),
        w[0],
        w[1],
        w[2],
        w[3],
        result,
        *ref_data,
        pair_sum,
        0,
        0,
        0,
    ]
}
