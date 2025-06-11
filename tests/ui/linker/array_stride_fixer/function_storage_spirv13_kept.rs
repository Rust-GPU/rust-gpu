// Test that ArrayStride decorations are kept for function storage in SPIR-V 1.3

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "\S*/lib/rustlib/" -> "$SYSROOT/lib/rustlib/"
// only-spv1.3
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut [u32; 1],
) {
    // Function storage in SPIR-V 1.3 should keep ArrayStride decorations
    let mut function_var: [u32; 256] = [0; 256];
    function_var[0] = 42;
    function_var[1] = function_var[0] + 1;
    // Force the array to be used by writing to output
    output[0] = function_var[1];
}
