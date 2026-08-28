// build-pass
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "^(; .*\n)*" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-spv1.4
// ignore-spv1.5
// ignore-spv1.6
// ignore-vulkan1.0
// ignore-vulkan1.1
use spirv_std::spirv;

fn do_work(data: &[u32], slab: &mut [u32]) {
    slab[0] = data[0];
    slab[1] = data[1];
    slab[2] = data[2];
}

#[spirv(compute(threads(64)))]
pub fn compute_shader(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] slab: &mut [u32],
    #[spirv(global_invocation_id)] global_id: glam::UVec3,
) {
    let data = [global_id.x, global_id.y, global_id.z];
    do_work(&data, slab);
}
