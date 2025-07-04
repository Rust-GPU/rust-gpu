#![no_std]
#![cfg_attr(target_arch = "spirv", feature(asm_experimental_arch))]

#[allow(unused_imports)]
use spirv_std::num_traits::Float;
use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[f32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [f32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    if tid < input.len() && tid < output.len() {
        let x = input[tid];

        // Test various trigonometric functions
        let result = match tid % 14 {
            0 => x.sin(),
            1 => x.cos(),
            2 => x.tan(),
            3 => x.asin().clamp(-1.0, 1.0), // Clamp to avoid NaN for values outside [-1, 1]
            4 => x.acos().clamp(-1.0, 1.0), // Clamp to avoid NaN for values outside [-1, 1]
            5 => x.atan(),
            6 => x.sinh(),
            7 => x.cosh(),
            8 => x.tanh(),
            9 => {
                // atan2 - use two consecutive values
                let y = if tid + 1 < input.len() {
                    input[tid + 1]
                } else {
                    1.0
                };
                x.atan2(y)
            }
            10 => (x * x + 1.0).sqrt(), // hypot equivalent: sqrt(x^2 + 1^2)
            11 => x.to_radians(),
            12 => x.to_degrees(),
            13 => {
                // sincos - return sin for even indices, cos for odd
                if tid % 2 == 0 { x.sin() } else { x.cos() }
            }
            _ => 0.0,
        };

        output[tid] = result;
    }
}
