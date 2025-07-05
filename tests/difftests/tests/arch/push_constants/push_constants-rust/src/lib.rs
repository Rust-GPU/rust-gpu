#![no_std]

#[allow(unused_imports)]
use spirv_std::num_traits::Float;
use spirv_std::spirv;

#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
pub struct PushConstants {
    multiplier: f32,
    offset: f32,
    flags: u32,
    count: u32,
}

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(push_constant)] push_constants: &PushConstants,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[f32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [f32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;
    let count = push_constants.count as usize;

    if tid < input.len() && tid < output.len() && tid < count {
        let value = input[tid];

        // Apply different operations based on flags
        let result = match push_constants.flags {
            0 => {
                // Linear transformation
                value * push_constants.multiplier + push_constants.offset
            }
            1 => {
                // Quadratic transformation
                value * value * push_constants.multiplier + push_constants.offset
            }
            2 => {
                // Sine wave modulation
                (value * push_constants.multiplier).sin() + push_constants.offset
            }
            3 => {
                // Exponential transformation
                (value * push_constants.multiplier).exp() + push_constants.offset
            }
            4 => {
                // Logarithmic transformation (with protection against negative values)
                if value > 0.0 {
                    (value * push_constants.multiplier).ln() + push_constants.offset
                } else {
                    push_constants.offset
                }
            }
            5 => {
                // Reciprocal transformation (with protection against division by zero)
                if value.abs() > 0.001 {
                    push_constants.multiplier / value + push_constants.offset
                } else {
                    push_constants.offset
                }
            }
            6 => {
                // Power transformation
                value.powf(push_constants.multiplier) + push_constants.offset
            }
            7 => {
                // Modulo operation (treating multiplier as divisor)
                if push_constants.multiplier > 0.0 {
                    (value % push_constants.multiplier) + push_constants.offset
                } else {
                    push_constants.offset
                }
            }
            _ => {
                // Default: just add offset
                value + push_constants.offset
            }
        };

        output[tid] = result;
    }
}
