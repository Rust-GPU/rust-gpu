#![no_std]

use spirv_std::spirv;

pub const WORDS_PER_CASE: usize = 16;

const VALUES: [u64; 2] = [0x9ABC_DEF0_1234_5678, 0x8000_0000_0000_0001];

const AMOUNTS: [u32; 14] = [0, 1, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, u32::MAX];

pub const NUM_CASES: usize = VALUES.len() * AMOUNTS.len();

pub fn test_data() -> [[u64; 2]; NUM_CASES] {
    let mut data = [[0; 2]; NUM_CASES];
    for (i, case) in data.iter_mut().enumerate() {
        *case = [VALUES[i / AMOUNTS.len()], AMOUNTS[i % AMOUNTS.len()] as u64];
    }
    data
}

pub fn rotate_case(value: u64, amount: u32) -> [u32; WORDS_PER_CASE] {
    let left_64 = value.rotate_left(amount);
    let right_64 = value.rotate_right(amount);
    let left_i64 = (value as i64).rotate_left(amount) as u64;
    let right_i64 = (value as i64).rotate_right(amount) as u64;
    // An amount that is a constant in the shader
    let left_64_const = value.rotate_left(40);
    [
        left_64 as u32,
        (left_64 >> 32) as u32,
        right_64 as u32,
        (right_64 >> 32) as u32,
        left_i64 as u32,
        (left_i64 >> 32) as u32,
        right_i64 as u32,
        (right_i64 >> 32) as u32,
        (value as u32).rotate_left(amount),
        (value as u32).rotate_right(amount),
        (value as u16).rotate_left(amount) as u32,
        (value as u16).rotate_right(amount) as u32,
        (value as u8).rotate_left(amount) as u32,
        (value as u8).rotate_right(amount) as u32,
        left_64_const as u32,
        (left_64_const >> 32) as u32,
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
        let words = rotate_case(input[2 * tid], input[2 * tid + 1] as u32);
        for i in 0..WORDS_PER_CASE {
            output[tid * WORDS_PER_CASE + i] = words[i];
        }
    }
}
