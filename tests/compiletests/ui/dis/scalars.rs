#![crate_name = "scalars"]

// Tests all the (supported) Rust integer/floating-point scalar types.

// revisions: supported nocaps
//[supported] build-pass
//[supported] compile-flags: -C target-feature=+Int8,+Int16,+Int64,+Float64
//[nocaps] build-fail

// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

// HACK(eddyb) `compiletest` handles `ui\dis\`, but not `ui\\dis\\`, on Windows.
// normalize-stderr-test "ui/dis/" -> "$$DIR/"

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    out: &mut u32,

    #[spirv(flat)] in_u8: u8,
    #[spirv(flat)] in_u16: u16,
    #[spirv(flat)] in_u32: u32,
    #[spirv(flat)] in_u64: u64,
    // FIXME(eddyb) support `u128` somehow!
    #[cfg(not(supported))]
    #[spirv(flat)]
    in_u128: u128,

    #[spirv(flat)] in_i8: i8,
    #[spirv(flat)] in_i16: i16,
    #[spirv(flat)] in_i32: i32,
    #[spirv(flat)] in_i64: i64,
    // FIXME(eddyb) support `i128` somehow!
    #[cfg(not(supported))]
    #[spirv(flat)]
    in_i128: i128,

    in_f32: f32,
    #[spirv(flat)] in_f64: f64,
) {
    // HACK(eddyb) to make it more obvious in the disassembly, each type gets a
    // constant with its bit width, disambiguated for signed integers by negation.
    *out |= (in_u8 * 8) as u32;
    *out |= (in_u16 * 16) as u32;
    *out |= (in_u32 * 32) as u32;
    *out |= (in_u64 * 64) as u32;
    // FIXME(eddyb) support `u128` somehow!
    #[cfg(not(supported))]
    {
        // FIXME(eddyb) constants still emit zombies that get reported too early.
        // *out |= (in_u128 * 128) as u32;
        *out |= (in_u128 + in_u128) as u32;
    }

    *out |= (in_i8 * -8) as u32;
    *out |= (in_i16 * -16) as u32;
    *out |= (in_i32 * -32) as u32;
    *out |= (in_i64 * -64) as u32;
    // FIXME(eddyb) support `i128` somehow!
    #[cfg(not(supported))]
    {
        // FIXME(eddyb) constants still emit zombies that get reported too early.
        // *out |= (in_i128 * -128) as u32;
        *out |= (in_i128 + in_i128) as u32;
    }

    *out |= (in_f32 * 32.0) as u32;
    *out |= (in_f64 * 64.0) as u32;
}
