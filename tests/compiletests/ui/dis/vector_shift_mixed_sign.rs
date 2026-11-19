// Test that a vector shift builds when the shift amount has another signedness
// than the shifted value. A shift by an amount of the same type is still fused
// into one vector instruction.

// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main
// normalize-stderr-test "\n\W*OpLine .*" -> ""

use spirv_std::glam::{IVec3, UVec3};
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(flat)] u: UVec3,
    #[spirv(flat)] i: IVec3,
    #[spirv(flat)] s: u32,
    out_u: &mut [UVec3; 2],
    out_i: &mut IVec3,
) {
    out_u[0] = u >> 1;
    *out_i = i << s;
    out_u[1] = u >> s;
}
