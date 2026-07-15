// Test `OpImageSampleImplicitLod` Bias
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "\n\W*OpSource .*" -> ""
// normalize-stderr-test "\n\W*OpLine .*" -> ""
// normalize-stderr-test "\n\W*%\d+ = OpString .*" -> ""
// normalize-stderr-test "\n\W*; .*" -> ""
// normalize-stderr-test "\n\W*OpCapability VulkanMemoryModel" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// build-pass

use spirv_std::spirv;
use spirv_std::{Image, Sampler, arch};

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image2d: &Image!(2D, type=f32, sampled),
    #[spirv(descriptor_set = 1, binding = 1)] image2d_array: &Image!(2D, type=f32, arrayed, sampled),
    #[spirv(descriptor_set = 2, binding = 2)] cubemap: &Image!(3D, type=f32, sampled),
    #[spirv(descriptor_set = 3, binding = 3)] sampler: &Sampler,
    output: &mut glam::Vec4,
) {
    let v2 = glam::Vec2::new(0.0, 1.0);
    let v3 = glam::Vec3::new(0.0, 1.0, 0.5);
    let r1: glam::Vec4 = image2d.sample_bias(*sampler, v2, 0.0);
    let r2: glam::Vec4 = image2d_array.sample_bias(*sampler, v3, 0.0);
    let r3: glam::Vec4 = cubemap.sample_bias(*sampler, v3, 0.0);
    *output = r1 + r2 + r3;
}
