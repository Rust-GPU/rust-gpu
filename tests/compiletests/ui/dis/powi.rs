// Test that `Float::powi` uses GLSL.std.450 Pow instead of a loop-based implementation.
// See https://github.com/Rust-GPU/rust-gpu/issues/516

// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std::num_traits::Float;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(input: f32, output: &mut f32) {
    *output = input.powi(2);
}
