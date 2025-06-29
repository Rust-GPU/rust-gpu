// compile-flags: -C llvm-args=--allow-fragment-no-output
// Test using `&&` operator.
// build-pass

use spirv_std::spirv;

fn f(x: bool, y: bool) -> bool {
    x && y
}

#[spirv(fragment)]
pub fn main() {
    f(false, true);
}
