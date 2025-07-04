#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    if tid >= output.len() {
        return; // Early return test
    }

    let val = if tid < input.len() { input[tid] } else { 0 };

    // Test 1: Simple if/else
    let result1 = if val % 2 == 0 { val * 2 } else { val * 3 };

    // Test 2: Nested if/else
    let result2 = if val < 10 {
        if val < 5 { val + 100 } else { val + 200 }
    } else if val < 20 {
        val + 300
    } else {
        val + 400
    };

    // Test 3: Loop with break
    let mut sum = 0u32;
    let mut i = 0;
    loop {
        if i >= val || i >= 10 {
            break;
        }
        sum += i;
        i += 1;
    }

    // Test 4: While loop
    let mut product = 1u32;
    let mut j = 1;
    while j <= 5 && j <= val {
        product *= j;
        j += 1;
    }

    // Test 5: For loop with continue
    let mut count = 0u32;
    for k in 0..20 {
        if k % 3 == 0 {
            continue;
        }
        if k > val {
            break;
        }
        count += 1;
    }

    // Test 6: Match expression
    let match_result = match val % 4 {
        0 => 1000,
        1 => 2000,
        2 => 3000,
        _ => 4000,
    };

    // Test 7: Complex control flow with early returns
    let complex_result = complex_function(val);

    // Combine all results
    output[tid] = result1
        .wrapping_add(result2)
        .wrapping_add(sum)
        .wrapping_add(product)
        .wrapping_add(count)
        .wrapping_add(match_result)
        .wrapping_add(complex_result);
}

fn complex_function(x: u32) -> u32 {
    if x == 0 {
        return 999;
    }

    for i in 0..x {
        if i > 5 {
            return i * 100;
        }
    }

    let mut result = x;
    while result > 10 {
        result /= 2;
        if result == 15 {
            return 777;
        }
    }

    result
}
