// build-pass
// only-vulkan1.2
// compile-flags: -Ctarget-feature=+MeshShadingEXT,+ext:SPV_EXT_mesh_shader

use spirv_std::arch::set_mesh_outputs_ext;
use spirv_std::glam::{UVec3, Vec4};
use spirv_std::spirv;

#[spirv(mesh_ext(
    threads(1),
    output_vertices = 3,
    output_primitives_ext = 1,
    output_triangles_ext
))]
pub fn main(
    #[spirv(position)] positions: &mut [Vec4; 3],
    #[spirv(primitive_triangle_indices_ext)] indices: &mut [UVec3; 1],
) {
    unsafe {
        set_mesh_outputs_ext(3, 1);
    }

    positions[0] = Vec4::new(-0.5, 0.5, 0.0, 1.0);
    positions[1] = Vec4::new(0.5, 0.5, 0.0, 1.0);
    positions[2] = Vec4::new(0.0, -0.5, 0.0, 1.0);

    indices[0] = UVec3::new(0, 1, 2);
}
