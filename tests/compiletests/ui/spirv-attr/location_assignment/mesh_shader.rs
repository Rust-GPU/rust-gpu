// build-pass
// compile-flags: -Ctarget-feature=+MeshShadingEXT,+ext:SPV_EXT_mesh_shader
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-vulkan1.0
// ignore-vulkan1.1

use spirv_std::arch::set_mesh_outputs_ext;
use spirv_std::glam::{UVec3, Vec4};
use spirv_std::spirv;

#[spirv(mesh_ext(
    threads(1),
    output_vertices = 9,
    output_primitives_ext = 3,
    output_triangles_ext
))]
pub fn main(
    #[spirv(position)] positions: &mut [Vec4; 9],
    #[spirv(primitive_triangle_indices_ext)] indices: &mut [UVec3; 3],
    // location 0
    out_per_vertex: &mut [u32; 9],
    // location 1
    out_per_vertex2: &mut [f32; 9],
    // location 2
    #[spirv(per_primitive_ext)] out_per_primitive: &mut [u32; 3],
    // location 3
    #[spirv(per_primitive_ext)] out_per_primitive2: &mut [f32; 3],
) {
    unsafe {
        set_mesh_outputs_ext(9, 3);
    }

    for i in 0..3 {
        positions[i * 3 + 0] = Vec4::new(-0.5, 0.5, 0.0, 1.0);
        positions[i * 3 + 1] = Vec4::new(0.5, 0.5, 0.0, 1.0);
        positions[i * 3 + 2] = Vec4::new(0.0, -0.5, 0.0, 1.0);
    }

    for i in 0..9 {
        out_per_vertex[i] = i as u32;
        out_per_vertex2[i] = i as f32;
    }

    for i in 0..3 {
        indices[i] = UVec3::new(0, 1, 2) + UVec3::splat(i as u32);
        out_per_primitive[i] = 42;
        out_per_primitive2[i] = 69.;
    }
}
