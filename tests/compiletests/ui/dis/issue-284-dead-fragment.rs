//@ compile-flags: --crate-type dylib --emit=metadata
// Test for issue #284: A fragment shader that produces no output should fail

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main_fs(#[spirv(flat)] in_color: u32, out_frag_color: &mut spirv_std::glam::Vec4) {
    // This fragment shader reads input but doesn't write output
    // The assignment is optimized away, causing no visible output
    let _temp = in_color;
    // Note: No assignment to out_frag_color, so this shader produces no output
}
