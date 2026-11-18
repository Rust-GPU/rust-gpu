#![no_std]

use spirv_std::spirv;

pub const WORDS_PER_CASE: usize = 10;

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

pub fn test(value: u64) -> [u32; WORDS_PER_CASE] {
    let (i32, i16, i8) = (value as i32, value as i16, value as i8);
    [
        i8 as i32 as u32,
        i8 as u32,
        i8 as i16 as u32,
        i8 as u16 as u32,
        i16 as i32 as u32,
        i16 as u32,
        (i32 as i64) as u32,
        (i32 as i64 >> 32) as u32,
        (i32 as u64) as u32,
        (i32 as u64 >> 32) as u32,
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
        let words = test(input[tid]);
        for i in 0..WORDS_PER_CASE {
            output[tid * WORDS_PER_CASE + i] = words[i];
        }
    }
}
