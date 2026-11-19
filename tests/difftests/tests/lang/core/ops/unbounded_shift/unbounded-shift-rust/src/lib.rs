#![no_std]

use spirv_std::spirv;

pub const WORDS_PER_CASE: usize = 24;

// The second value is negative for every signed integer width
const VALUES: [u64; 2] = [0x9ABC_DEF0_1234_5678, 0x8000_0000_8000_8081];

const AMOUNTS: [u32; 15] = [0, 1, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, u32::MAX];

pub const NUM_CASES: usize = VALUES.len() * AMOUNTS.len();

pub fn test_data() -> [[u64; 2]; NUM_CASES] {
    let mut data = [[0; 2]; NUM_CASES];
    for (i, case) in data.iter_mut().enumerate() {
        *case = [VALUES[i / AMOUNTS.len()], AMOUNTS[i % AMOUNTS.len()] as u64];
    }
    data
}

pub fn unbounded_shift_case(value: u64, amount: u32) -> [u32; WORDS_PER_CASE] {
    let shl_64 = value.unbounded_shl(amount);
    let shr_64 = value.unbounded_shr(amount);
    let shl_i64 = (value as i64).unbounded_shl(amount) as u64;
    let shr_i64 = (value as i64).unbounded_shr(amount) as u64;
    // Amounts that are constants in the shader, in and out of range
    let shl_64_const = value.unbounded_shl(40);
    let shr_64_const = value.unbounded_shr(64);
    let shr_i64_const = (value as i64).unbounded_shr(64) as u64;
    [
        shl_64 as u32,
        (shl_64 >> 32) as u32,
        shr_64 as u32,
        (shr_64 >> 32) as u32,
        shl_i64 as u32,
        (shl_i64 >> 32) as u32,
        shr_i64 as u32,
        (shr_i64 >> 32) as u32,
        (value as u32).unbounded_shl(amount),
        (value as u32).unbounded_shr(amount),
        (value as i32).unbounded_shl(amount) as u32,
        (value as i32).unbounded_shr(amount) as u32,
        (value as u16).unbounded_shl(amount) as u32,
        (value as u16).unbounded_shr(amount) as u32,
        (value as i16).unbounded_shr(amount) as u32,
        (value as u8).unbounded_shl(amount) as u32,
        (value as u8).unbounded_shr(amount) as u32,
        (value as i8).unbounded_shr(amount) as u32,
        shl_64_const as u32,
        (shl_64_const >> 32) as u32,
        shr_64_const as u32,
        shr_i64_const as u32,
        (value as i8).unbounded_shr(8) as u32,
        (value as u8).unbounded_shl(4) as u32,
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
        let words = unbounded_shift_case(input[2 * tid], input[2 * tid + 1] as u32);
        for i in 0..WORDS_PER_CASE {
            output[tid * WORDS_PER_CASE + i] = words[i];
        }
    }
}
