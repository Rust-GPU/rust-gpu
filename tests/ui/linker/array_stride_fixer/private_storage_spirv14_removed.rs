// Test that ArrayStride decorations are removed from private storage in SPIR-V 1.4

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "\S*/lib/rustlib/" -> "$SYSROOT/lib/rustlib/"
// only-spv1.4
use spirv_std::spirv;

// Helper function to create an array in private storage
fn create_private_array() -> [u32; 4] {
    [0, 1, 2, 3]
}

#[spirv(compute(threads(1)))]
pub fn main() {
    // This creates a private storage array in SPIR-V 1.4+
    // ArrayStride decorations should be removed
    let mut private_array = create_private_array();
    private_array[0] = 42;
}
