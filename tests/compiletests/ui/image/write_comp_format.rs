// build-pass
// compile-flags: -C target-feature=+StorageImageExtendedFormats
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-vulkan1.0
// ignore-vulkan1.1
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3

use spirv_std::glam::*;
use spirv_std::spirv;
use spirv_std::{Image, arch};

#[spirv(compute(threads(8, 8)))]
pub fn main(
    #[spirv(global_invocation_id)] global_id: UVec3,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] fill_color: &Vec2,
    #[spirv(descriptor_set = 0, binding = 1)] image: &Image!(2D, format = rg32f, sampled = false),
) {
    unsafe {
        image.write(global_id.xy(), *fill_color);
    }
}
