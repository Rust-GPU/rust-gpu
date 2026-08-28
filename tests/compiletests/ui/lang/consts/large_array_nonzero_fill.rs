// Tests that large non-zero-initialized arrays emit a compile warning.
// A `[v; N]` array with non-zero v generates N SPIR-V instructions (either
// store ops or OpConstantComposite operands), which is inherently slow.

// build-pass

#![no_std]
use spirv_std::glam::Vec4;
use spirv_std::spirv;

#[spirv(vertex)]
pub fn test_vs(#[spirv(push_constant)] index: &u32, #[spirv(position)] out_pos: &mut Vec4) {
    let nonzeroed = [1.0f32; 2048];

    let i = *index as usize % 2048;
    *out_pos = Vec4::new(nonzeroed[i], nonzeroed[i], 0.0, 1.0);
}
