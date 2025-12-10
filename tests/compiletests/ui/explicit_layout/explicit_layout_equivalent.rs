// build-pass
// compile-flags: -C llvm-args=--disassemble
// compile-flags: -C target-feature=+Int8
// compile-flags: -C target-feature=+Int16
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

use spirv_std::arch::IndexUnchecked;
use spirv_std::glam::*;
use spirv_std::prototype::MyStruct;
use spirv_std::{Image, spirv};

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] input: &[u32],
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] output: &mut [u32],
    #[spirv(local_invocation_index)] inv_id: UVec3,
) {
    const {
        assert!(size_of::<MyStruct>() == 16);
    }
    // offset in bytes
    let offset = inv_id.x as usize * 16;
    unsafe {
        *output.index_unchecked_mut(offset) = *input.index_unchecked(offset);
        *output.index_unchecked_mut(offset + 1) = *input.index_unchecked(offset + 1);
        *output.index_unchecked_mut(offset + 2) = *input.index_unchecked(offset + 2);
        *output.index_unchecked_mut(offset + 3) = *input.index_unchecked(offset + 3) & 0xFFFF00FF;
    }
}

// rga analysis and glsl:
// clear && cargo compiletest explicit_layout --bless && cp ../target/compiletest-results/explicit_layout.vulkan1.2 ./explicit_layout.spv && rga -s vulkan -c gfx1032 --comp explicit_layout.spv -a analysis.txt --livereg vgpr.txt --livereg-sgpr sgpr.txt && spirv-cross -V ./explicit_layout.spv >./explicit_layout.glsl
