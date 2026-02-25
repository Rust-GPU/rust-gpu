// build-pass
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

use spirv_std::glam::*;
use spirv_std::{Image, spirv};

#[derive(Copy, Clone, Default)]
pub struct LargerThanVec4 {
    a: Vec4,
    b: Vec2,
}

#[spirv(vertex)]
pub fn main(
    #[spirv(location = 4)] out1: &mut LargerThanVec4,
    out2: &mut Vec2, // should be 6
    #[spirv(location = 0)] out3: &mut Mat4,
    // 8 to 11 are unused, that's fine
    #[spirv(location = 12)] out4: &mut f32,
) {
    *out1 = Default::default();
    *out2 = Default::default();
    *out3 = Default::default();
    *out4 = Default::default();
}
