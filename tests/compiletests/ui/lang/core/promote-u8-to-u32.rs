// build-pass
//PartialOrd on CustomPosition(u32) internally returns Option<Ordering>,
//where Ordering is represented as i8 in Rust's layout.
//This caused rust-gpu to emit OpTypeInt 8 declarations requiring OpCapability Int8
#![no_std]

use spirv_std::{glam::Vec4, spirv};

pub struct ShaderInputs {
    pub x: CustomPosition,
    pub y: CustomPosition,
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub struct CustomPosition(u32);

#[spirv(vertex)]
pub fn test_vs(
    #[spirv(push_constant)] inputs: &ShaderInputs,
    #[spirv(position)] out_pos: &mut Vec4,
) {
    let mut result: f32 = 0.;
    if inputs.x < inputs.y {
        result = 1.0;
    }
    *out_pos = Vec4::new(inputs.x.0 as f32, inputs.y.0 as f32, result as f32, 1.0);
}
