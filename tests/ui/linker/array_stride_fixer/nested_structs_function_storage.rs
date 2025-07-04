// Test that ArrayStride decorations are removed from nested structs with arrays in Function storage class

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// only-vulkan1.2
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "\S*/lib/rustlib/" -> "$SYSROOT/lib/rustlib/"
use spirv_std::spirv;

#[derive(Copy, Clone)]
struct InnerStruct {
    data: [f32; 4],
}

#[derive(Copy, Clone)]
struct OuterStruct {
    inner: InnerStruct,
}

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut [f32; 1],
) {
    // Function-local variables with nested structs containing arrays
    // Should have ArrayStride removed in SPIR-V 1.4+
    let mut function_var = OuterStruct {
        inner: InnerStruct { data: [0.0; 4] },
    };
    function_var.inner.data[0] = 42.0;
    function_var.inner.data[1] = function_var.inner.data[0] + 1.0;
    // Force usage to prevent optimization
    output[0] = function_var.inner.data[1];
}
