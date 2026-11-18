// Test that widening integer casts extend based on the signedness of the source type,
// not the destination type.

// build-pass
// compile-flags: -C target-feature=+Int8,+Int64
// compile-flags: -C llvm-args=--disassemble-fn=int_cast_extension::casts
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "\n\W*OpCapability VulkanMemoryModel" -> ""
// normalize-stderr-test "\n\W*OpSource .*" -> ""
// normalize-stderr-test "\n\W*OpExtension .SPV_KHR_vulkan_memory_model." -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// normalize-stderr-test "OpString .*" -> "OpString"

use spirv_std::spirv;

const MINUS_ONE: i8 = -1;
const MINUS_TWO: i8 = -2;

#[inline(never)]
fn casts(a: i8, b: u8, c: i32, d: u32) -> [u64; 6] {
    [
        // sign extension to an unsigned type
        a as u32 as u64,
        // zero extension to a signed type
        b as i32 as u64,
        c as u64,
        d as i64 as u64,
        // constant sign extension to an unsigned type
        MINUS_ONE as u64,
        // constant sign extension through `From`
        i64::from(MINUS_TWO) as u64,
    ]
}

#[spirv(fragment)]
pub fn main(#[spirv(flat)] x: u32, out: &mut [u64; 6]) {
    *out = casts(x as i8, x as u8, x as i32, x);
}
