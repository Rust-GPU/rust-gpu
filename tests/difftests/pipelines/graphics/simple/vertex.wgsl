struct VertexOutput {
    @builtin(position) out_pos: vec4<f32>,
};

@vertex
fn main_vs(@builtin(vertex_index) vert_id: u32) -> VertexOutput {
    let id: i32 = i32(vert_id);
    let x: f32 = f32(id - 1);
    let y: f32 = f32((id & 1) * 2 - 1);
    return VertexOutput(vec4<f32>(x, y, 0.0, 1.0));
}
