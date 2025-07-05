#![no_std]

use difftest::round6;
#[allow(unused_imports)]
use spirv_std::num_traits::Float;
use spirv_std::spirv;

#[spirv(compute(threads(32)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[f32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [f32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    if tid >= 32 || tid >= input.len() {
        return;
    }

    let x = input[tid];
    let base_offset = tid * 21;

    if base_offset + 20 >= output.len() {
        return;
    }

    // Basic arithmetic
    output[base_offset + 0] = round6!(x + 1.5);
    output[base_offset + 1] = round6!(x - 0.5);
    output[base_offset + 2] = round6!(x * 2.0);
    output[base_offset + 3] = round6!(x / 2.0);
    output[base_offset + 4] = round6!(x % 3.0);

    // Trigonometric functions (simplified for consistent results)
    output[base_offset + 5] = round6!(x.sin());
    output[base_offset + 6] = round6!(x.cos());
    output[base_offset + 7] = round6!(x.tan().clamp(-10.0, 10.0));
    output[base_offset + 8] = 0.0;
    output[base_offset + 9] = 0.0;
    output[base_offset + 10] = round6!(x.atan());

    // Exponential and logarithmic (simplified)
    output[base_offset + 11] = round6!(x.exp().min(1e6));
    output[base_offset + 12] = round6!(if x > 0.0 { x.ln() } else { -10.0 });
    output[base_offset + 13] = round6!(x.abs().sqrt());
    output[base_offset + 14] = round6!(x.abs() * x.abs()); // Use multiplication instead of powf
    output[base_offset + 15] = round6!(if x > 0.0 { x.log2() } else { -10.0 });
    output[base_offset + 16] = round6!(x.exp2().min(1e6));
    output[base_offset + 17] = x.floor(); // floor/ceil/round are exact
    output[base_offset + 18] = x.ceil();
    output[base_offset + 19] = x.round();

    // Special values and conversions
    let int_val = x as i32;
    output[base_offset + 20] = int_val as f32;
}
