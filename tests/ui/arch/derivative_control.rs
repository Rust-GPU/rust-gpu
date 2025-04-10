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
    Derivative::ddx_fine(0.);
    Derivative::ddy_fine(0.);
    Derivative::fwidth_fine(0.);

    Derivative::ddx_coarse(0.);
    Derivative::ddy_coarse(0.);
    Derivative::fwidth_coarse(0.);
}
