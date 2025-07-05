#![no_std]
#![cfg_attr(target_arch = "spirv", feature(asm_experimental_arch))]

use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    if tid < input.len() && tid < output.len() {
        let value = input[tid];

        // Complex control flow with nested loops, early returns, and match expressions
        let result = process_value(value, tid as u32);

        output[tid] = result;
    }
}

fn process_value(value: u32, tid: u32) -> u32 {
    // Early return for special cases
    if value == 0 {
        return 0;
    }

    if value > 1000 {
        return value - 1000;
    }

    // Complex match expression
    let base = match value % 10 {
        0 => 10,
        1 | 2 => value * 2,
        3..=5 => {
            // Nested computation
            let mut sum = 0;
            for i in 0..3 {
                sum += value + i;
            }
            sum
        }
        6 => {
            // Early return from match arm
            if tid % 2 == 0 {
                return value * 3;
            }
            value * 4
        }
        7 | 8 => {
            // Nested loop with break
            let mut result = 0;
            'outer: for i in 0..5 {
                for j in 0..3 {
                    result += i * j;
                    if result > value {
                        break 'outer;
                    }
                }
            }
            result
        }
        9 => {
            // Complex nested condition
            if tid < 10 {
                if value < 50 { value + tid } else { value - tid }
            } else {
                // Loop with continue
                let mut sum = 0;
                for i in 0..value {
                    if i % 3 == 0 {
                        continue;
                    }
                    sum += i;
                    if sum > 100 {
                        break;
                    }
                }
                sum
            }
        }
        _ => value, // This should never be reached due to modulo 10
    };

    // Final transformation with nested conditions
    if base > 50 {
        if tid % 3 == 0 {
            base / 2
        } else if tid % 3 == 1 {
            base * 2
        } else {
            base
        }
    } else {
        // Another loop with multiple exit conditions
        let mut result = base;
        let mut counter = 0;
        loop {
            result = (result * 3 + 1) / 2;
            counter += 1;

            if result == 1 || counter >= 10 || result > 1000 {
                break;
            }
        }
        result + counter
    }
}
