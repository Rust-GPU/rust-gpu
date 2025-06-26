// compile-flags: -C llvm-args=--allow-fragment-no-output
// Test that calling `panic!` works.
// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    panic!("aaa");
}
