// Test black_box intrinsic
// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=black_box::disassemble

#![allow(internal_features)]
#![feature(core_intrinsics)]
#![no_std]

use core::hint::black_box;
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut [u32]) {
    let result = disassemble();
    for i in 0..result.len() {
        out[i] = result[i];
    }
}

pub fn disassemble() -> [u32; 8] {
    // Test with various types
    let x = 42i32;
    let y = black_box(x);

    let a = 3.14f32;
    let b = black_box(a);

    let v = [1, 2, 3, 4];
    let w = black_box(v);

    // Test in expressions
    let result = black_box(10) + black_box(20);

    // Test with references
    let data = 100u32;
    let ref_data = black_box(&data);

    [
        y as u32,
        f32::to_bits(b),
        w[0],
        w[1],
        w[2],
        w[3],
        result,
        *ref_data,
    ]
}
