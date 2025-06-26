// Simple single entrypoint function test.
// build-pass
// compile-flags: -C llvm-args=--allow-fragment-no-output

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {}
