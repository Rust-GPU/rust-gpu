// build-pass
// compile-flags: -Ctarget-feature=+Geometry
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

use spirv_std::arch::{emit_vertex, end_primitive};
use spirv_std::glam::*;
use spirv_std::spirv;

pub struct Attr1 {
    pub a: Vec4,
    pub b: Vec2,
    pub c: f32,
}

pub struct Attr2 {
    pub d: f32,
}

#[spirv(geometry(input_points = 2, output_line_strip = 2))]
pub fn main(
    // #[spirv(descriptor_set = 0, binding = 0, storage_buffer)]
    #[spirv(position)] position_in: Vec4,
    #[spirv(position)] position_out: &mut Vec4,
    // location 0
    attr1_in: [f32; 2],
    // location 0
    attr1_out: &mut f32,
    // location 1
    attr2_in: [u32; 2],
    // location 1
    attr2_out: &mut u32,
) {
    unsafe {
        *attr1_out = attr1_in[0];
        *attr2_out = attr2_in[0];
        *position_out = position_in + vec4(-0.1, 0.0, 0.0, 0.0);
        emit_vertex();

        *attr1_out = attr1_in[1];
        *attr2_out = attr2_in[1];
        *position_out = position_in + vec4(0.1, 0.0, 0.0, 0.0);
        emit_vertex();

        end_primitive();
    };
}
