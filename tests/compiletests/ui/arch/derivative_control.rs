// build-pass
// compile-flags: -C target-feature=+DerivativeControl
// compile-flags: -C llvm-args=--disassemble-fn=derivative_control::derivative

use spirv_std::arch::Derivative;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    derivative();
}

pub fn derivative() {
    Derivative::dfdx_fine(0.);
    Derivative::dfdy_fine(0.);
    Derivative::fwidth_fine(0.);

    Derivative::dfdx_coarse(0.);
    Derivative::dfdy_coarse(0.);
    Derivative::fwidth_coarse(0.);
}
