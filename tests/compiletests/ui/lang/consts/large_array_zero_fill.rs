// Tests that zero-initialized large arrays compile without warnings.
// A `[0; N]` array goes through `memset_const_pattern` with fill_byte=0,
// which should emit a single OpConstantNull

// build-pass

#![no_std]
use spirv_std::glam::Vec4;
use spirv_std::spirv;

#[spirv(vertex)]
pub fn test_vs(#[spirv(push_constant)] index: &u32, #[spirv(position)] out_pos: &mut Vec4) {
    let zeroed = [0.0f32; 2048];

    let i = *index as usize % 2048;
    *out_pos = Vec4::new(zeroed[i], zeroed[i], 0.0, 1.0);
}
