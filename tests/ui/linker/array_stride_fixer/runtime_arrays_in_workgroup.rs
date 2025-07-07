// Test that ArrayStride decorations are removed from runtime arrays in Workgroup storage class

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// only-vulkan1.1
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "\S*/lib/rustlib/" -> "$SYSROOT/lib/rustlib/"
use spirv_std::RuntimeArray;
use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut [u32; 1],
    #[spirv(workgroup)] shared_array: &mut [u32; 256],
) {
    // Workgroup arrays should have ArrayStride removed
    shared_array[0] = 42;
    shared_array[1] = shared_array[0] + 1;
    // Force usage to prevent optimization
    output[0] = shared_array[1];
}
