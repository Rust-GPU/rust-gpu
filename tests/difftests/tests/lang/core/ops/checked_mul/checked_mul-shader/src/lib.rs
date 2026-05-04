#![no_std]

use spirv_std::spirv;

// 8 (a, b) input pairs flattened as `[a0, b0, a1, b1, ...]`. Each pair is
// multiplied as both `u32` and `i32` (reinterpreting the bits) and the four
// resulting words are written contiguously: `[u_low, u_overflow, s_low, s_overflow]`.
//
// Cases include a mix of non-overflowing and overflowing products in both
// signed and unsigned interpretations, including signed `i32::MIN * i32::MIN`
// (which overflows).
pub const PAIR_COUNT: usize = 8;
pub const INPUT_LEN: usize = PAIR_COUNT * 2;
pub const OUTPUT_LEN: usize = PAIR_COUNT * 4;

#[cfg(not(target_arch = "spirv"))]
#[rustfmt::skip]
pub const INPUTS: [u32; INPUT_LEN] = [
    0,           0,
    1,           1,
    1000,        1000,
    0x0000_FFFF, 0x0000_FFFF,
    0x0001_0000, 0x0001_0000,
    0xFFFF_FFFF, 2,
    0x8000_0000, 0x8000_0000,
    0x1234_5678, 0xABCD_EF01,
];

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u32; INPUT_LEN],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32; OUTPUT_LEN],
) {
    let mut i = 0;
    while i < PAIR_COUNT {
        let a = input[2 * i];
        let b = input[2 * i + 1];
        let (ur, uo) = a.overflowing_mul(b);
        output[4 * i] = ur;
        output[4 * i + 1] = uo as u32;
        let (sr, so) = (a as i32).overflowing_mul(b as i32);
        output[4 * i + 2] = sr as u32;
        output[4 * i + 3] = so as u32;
        i += 1;
    }
}
