// Test that mixed storage class usage results in proper ArrayStride handling

// compile-flags: -C llvm-args=--disassemble-globals
// only-vulkan1.1
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "\S*/lib/rustlib/" -> "$SYSROOT/lib/rustlib/"
use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] storage_data: &mut [u32; 256],
    #[spirv(workgroup)] workgroup_data: &mut [u32; 256],
) {
    // Both variables use the same array type [u32; 256] but in different storage classes:
    // - storage_data is in StorageBuffer (requires ArrayStride)
    // - workgroup_data is in Workgroup (forbids ArrayStride in SPIR-V 1.4+)

    storage_data[0] = 42;
    workgroup_data[0] = storage_data[0];
}
