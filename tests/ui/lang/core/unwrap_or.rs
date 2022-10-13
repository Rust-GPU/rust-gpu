// unwrap_or generates some memory-bools (as u8). Test to make sure they're fused away.
// OpINotEqual, as well as %bool, should not appear in the output.

// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn main(#[rust_gpu::spirv(flat)] out: &mut u32) {
    *out = None.unwrap_or(15);
}
