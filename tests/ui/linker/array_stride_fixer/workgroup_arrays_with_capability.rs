// Test that ArrayStride decorations are kept for arrays in Workgroup storage class with WorkgroupMemoryExplicitLayoutKHR capability

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals -Ctarget-feature=+WorkgroupMemoryExplicitLayoutKHR,+ext:SPV_KHR_workgroup_memory_explicit_layout
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "\S*/lib/rustlib/" -> "$SYSROOT/lib/rustlib/"
// only-vulkan1.2

use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut [u32; 1],
    #[spirv(workgroup)] shared_data: &mut [u32; 256],
) {
    // With WorkgroupMemoryExplicitLayoutKHR capability, ArrayStride should be kept
    shared_data[0] = 42;
    shared_data[1] = shared_data[0] + 1;
    // Force usage to prevent optimization
    output[0] = shared_data[1];
}
