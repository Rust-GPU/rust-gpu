// Test that ArrayStride decorations are kept for arrays in StorageBuffer storage class

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// only-vulkan1.1
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "\S*/lib/rustlib/" -> "$SYSROOT/lib/rustlib/"
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] storage_buffer_var: &mut [u32; 256],
) {
    // StorageBuffer storage class should keep ArrayStride decorations
    storage_buffer_var[0] = 42;
}
