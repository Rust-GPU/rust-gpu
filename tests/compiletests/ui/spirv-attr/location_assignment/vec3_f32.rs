// build-pass
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "\n\W*OpSource .*" -> ""
// normalize-stderr-test "\n\W*OpLine .*" -> ""
// normalize-stderr-test "\n\W*%\d+ = OpString .*" -> ""
// normalize-stderr-test "\n\W*; .*" -> ""
// normalize-stderr-test "\n\W*OpCapability VulkanMemoryModel" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-vulkan1.0
// ignore-vulkan1.1

use spirv_std::glam::*;
use spirv_std::{Image, spirv};

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct Vec3AndFloat {
    a: Vec3,
    b: f32,
}

#[spirv(vertex)]
pub fn main(out1: &mut Vec3AndFloat, out2: &mut f32) {
    const {
        assert!(size_of::<Vec3AndFloat>() == 16);
    }
    *out1 = Default::default();
    *out2 = Default::default();
}
