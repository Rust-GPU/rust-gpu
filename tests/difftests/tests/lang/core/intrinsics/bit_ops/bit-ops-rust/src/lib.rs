#![no_std]

use spirv_std::spirv;

pub const WORDS_PER_CASE: usize = 26;

pub const TEST_DATA: [u64; 16] = [
    0x0000000000000000,
    0x0000000000000001,
    0x0000000000000010,
    0x0000000000000080,
    0x00000000000000FF,
    0x0000000000000100,
    0x0000000000008000,
    0x000000000000FFFF,
    0x0000000080000000,
    0x00000000FFFFFFFF,
    0x0000000100000000,
    0x0010000000000000,
    0x8000000000000000,
    0x8000000000000001,
    0x9ABCDEF012345678,
    0xFFFFFFFFFFFFFFFF,
];

pub fn leading_zeros_case(value: u64) -> [u32; WORDS_PER_CASE] {
    let (v64, v32, v16, v8) = (value, value as u32, value as u16, value as u8);
    let v64_next_power_of_two = v64.checked_next_power_of_two().unwrap_or(0);
    let v64_swap_bytes = v64.swap_bytes();
    [
        v64.count_ones(),
        v32.count_ones(),
        v16.count_ones(),
        v8.count_ones(),
        v64_swap_bytes as u32,
        (v64_swap_bytes >> 32) as u32,
        v32.swap_bytes(),
        v16.swap_bytes() as u32,
        v8.swap_bytes() as u32,
        v64.leading_zeros(),
        v32.leading_zeros(),
        v16.leading_zeros(),
        v8.leading_zeros(),
        v64.trailing_zeros(),
        v32.trailing_zeros(),
        v16.trailing_zeros(),
        v8.trailing_zeros(),
        // `checked_ilog2` counts the leading zeros of a value that is known to be non-zero
        v64.checked_ilog2().unwrap_or(u32::MAX),
        v32.checked_ilog2().unwrap_or(u32::MAX),
        v16.checked_ilog2().unwrap_or(u32::MAX),
        v8.checked_ilog2().unwrap_or(u32::MAX),
        // The 64-bit result takes two words, low word first
        v64_next_power_of_two as u32,
        (v64_next_power_of_two >> 32) as u32,
        v32.checked_next_power_of_two().unwrap_or(0),
        v16.checked_next_power_of_two().unwrap_or(0) as u32,
        v8.checked_next_power_of_two().unwrap_or(0) as u32,
    ]
}

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u64],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    if tid < input.len() {
        let words = leading_zeros_case(input[tid]);
        for i in 0..WORDS_PER_CASE {
            output[tid * WORDS_PER_CASE + i] = words[i];
        }
    }
}
