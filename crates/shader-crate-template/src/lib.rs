//! Shader entry points.
//!
//! Contains an example vertex shader, fragment shader.
#![no_std]
use spirv_std::glam::{Vec2, Vec4};
use spirv_std::spirv;

pub const CLIP_SPACE_COORD_QUAD_CCW: [Vec4; 6] = {
    let tl = Vec4::new(-1.0, 1.0, 0.5, 1.0);
    let tr = Vec4::new(1.0, 1.0, 0.5, 1.0);
    let bl = Vec4::new(-1.0, -1.0, 0.5, 1.0);
    let br = Vec4::new(1.0, -1.0, 0.5, 1.0);
    [bl, br, tr, tr, tl, bl]
};

pub const UV_COORD_QUAD_CCW: [Vec2; 6] = {
    let tl = Vec2::new(0.0, 0.0);
    let tr = Vec2::new(1.0, 0.0);
    let bl = Vec2::new(0.0, 1.0);
    let br = Vec2::new(1.0, 1.0);
    [bl, br, tr, tr, tl, bl]
};

/// Vertex shader that renders an implicit quad.
#[spirv(vertex)]
pub fn vertex(
    #[spirv(vertex_index)] vertex_id: u32,
    out_uv: &mut Vec2,
    #[spirv(position)] clip_pos: &mut Vec4,
) {
    let index = vertex_id as usize % 6;
    *out_uv = UV_COORD_QUAD_CCW[index];
    *clip_pos = CLIP_SPACE_COORD_QUAD_CCW[index];
}

/// Fragment shader that uses UV coords passed in from the vertex shader
/// to render a simple gradient.
#[spirv(fragment)]
pub fn fragment(in_uv: Vec2, frag_color: &mut Vec4) {
    *frag_color = Vec4::new(in_uv.x, in_uv.y, 0.0, 1.0);
}
