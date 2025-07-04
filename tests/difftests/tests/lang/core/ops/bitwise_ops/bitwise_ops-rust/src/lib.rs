#![no_std]
#![cfg_attr(target_arch = "spirv", feature(asm_experimental_arch))]

use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input_a: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] input_b: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 2)] output: &mut [u32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    if tid < input_a.len() && tid < input_b.len() && tid < output.len() {
        let a = input_a[tid];
        let b = input_b[tid];

        // Test various bitwise operations
        let result = match tid % 12 {
            0 => a & b,                  // AND
            1 => a | b,                  // OR
            2 => a ^ b,                  // XOR
            3 => !a,                     // NOT
            4 => a << (b % 32),          // Left shift (avoid UB with modulo)
            5 => a >> (b % 32),          // Right shift (avoid UB with modulo)
            6 => a.rotate_left(b % 32),  // Rotate left
            7 => a.rotate_right(b % 32), // Rotate right
            8 => a.count_ones(),         // Population count
            9 => a.leading_zeros(),      // Leading zeros
            10 => a.trailing_zeros(),    // Trailing zeros
            11 => a.reverse_bits(),      // Bit reversal
            _ => 0,
        };

        output[tid] = result;
    }
}
