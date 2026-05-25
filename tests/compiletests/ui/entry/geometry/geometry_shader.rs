// build-pass
// compile-flags: -Ctarget-feature=+Geometry
// compile-flags: -C llvm-args=--disassemble
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

use spirv_std::geometry::{emit_vertex, end_primitive};
use spirv_std::glam::*;
use spirv_std::spirv;

#[spirv(geometry(input_points = 2, output_triangle_strip = 1))]
pub fn main(#[spirv(position)] position_in: [Vec4; 2], #[spirv(position)] position_out: &mut Vec4) {
    unsafe {
        *position_out = position_in[0] + vec4(-0.1, 0.0, 0.0, 0.0);
        emit_vertex();
        *position_out = position_in[1] + vec4(0.1, 0.0, 0.0, 0.0);
        emit_vertex();
        *position_out = position_in[1] + vec4(0.0, 0.1, 0.0, 0.0);
        emit_vertex();
        end_primitive();
    };
}
