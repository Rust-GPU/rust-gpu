// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// compile-flags: -C target-feature=+DrawParameters
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-vulkan1.0
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2

use spirv_std::glam::*;
use spirv_std::spirv;

#[spirv(vertex)]
pub fn vertex(
    #[spirv(position)] position_out: &mut Vec4,
    #[spirv(vertex_index)] vertex_index: u32,
    #[spirv(instance_index)] instance_index: u32,
    #[spirv(base_vertex)] base_vertex: u32,
    #[spirv(base_instance)] base_instance: u32,
    #[spirv(draw_index)] draw_index: u32,
) {
    *position_out = Vec4::new(
        vertex_index as f32,
        instance_index as f32,
        base_vertex as f32,
        base_instance as f32 + draw_index as f32,
    );
}
