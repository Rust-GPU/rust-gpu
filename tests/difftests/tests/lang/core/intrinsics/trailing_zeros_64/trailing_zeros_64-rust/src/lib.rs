#![no_std]
#![cfg_attr(target_arch = "spirv", feature(asm_experimental_arch))]

use spirv_std::spirv;

/// Test cases for 64-bit trailing_zeros - shared between GPU and CPU tests
#[cfg(not(target_arch = "spirv"))]
pub const TEST_DATA: [u64; 16] = [
    0x0000000000000000,
    0x0000000000000001,
    0x8000000000000000,
    0xFFFFFFFFFFFFFFFE,
    0x1234000000000000,
    0x0000000100000000,
    0x0000000000001000,
    0x0000000080000000,
    0x0000000000000010,
    0x0000000000000100,
    0x0000000000010000,
    0x0001000000000000,
    0x0100000000000000,
    0xFFFFFFFFFFFFFFFF,
    0x8000000000000001,
    0x4000000000000000,
];

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u64],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    if tid < input.len() && tid < output.len() {
        output[tid] = input[tid].trailing_zeros();
    }
}
