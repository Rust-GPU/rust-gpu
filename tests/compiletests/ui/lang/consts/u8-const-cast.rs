// build-pass
// Test that u8 constants cast to u32 don't require Int8 capability

use spirv_std::spirv;

const K: u8 = 20;

#[spirv(fragment)]
pub fn main(output: &mut u32) {
    let position = 2u32;
    // This should not require Int8 capability as K is only used as u32
    let global_y_offset_bits = position * K as u32;
    *output = global_y_offset_bits;
}

#[spirv(fragment)]
pub fn test_various_const_casts(output: &mut [u32; 5]) {
    // Test u8 -> u32
    const U8_VAL: u8 = 255;
    output[0] = U8_VAL as u32;

    // Test i8 -> i32
    const I8_VAL: i8 = -128;
    output[1] = I8_VAL as i32 as u32;

    // Test u16 -> u32
    const U16_VAL: u16 = 65535;
    output[2] = U16_VAL as u32;

    // Test bool -> u32
    const BOOL_VAL: bool = true;
    output[3] = BOOL_VAL as u32;

    // Test u32 -> bool -> u32
    const U32_VAL: u32 = 42;
    output[4] = (U32_VAL != 0) as u32;
}
