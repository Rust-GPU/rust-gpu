// Test black_box intrinsic
// build-pass

#![allow(internal_features)]
#![feature(core_intrinsics)]
#![no_std]

use core::hint::black_box;
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main() {
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
}
