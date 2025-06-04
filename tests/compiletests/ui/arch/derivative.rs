// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=derivative::derivative

use spirv_std::arch::Derivative;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    derivative();
}

pub fn derivative() {
    Derivative::dfdx(0.);
    Derivative::dfdy(0.);
    Derivative::fwidth(0.);
}
