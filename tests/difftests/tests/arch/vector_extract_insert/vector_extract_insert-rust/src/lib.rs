#![no_std]
#![cfg_attr(target_arch = "spirv", feature(asm_experimental_arch))]

use glam::Vec4;
use spirv_std::arch::{vector_extract_dynamic, vector_insert_dynamic};
use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[[f32; 4]],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] indices: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 2)] output: &mut [[f32; 4]],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    if tid < input.len() && tid < indices.len() && tid < output.len() {
        let vec = Vec4::from_array(input[tid]);
        let index = (indices[tid] % 4) as usize; // Ensure index is within bounds

        // Test various extract and insert operations
        let result = match tid % 8 {
            0 => {
                // Extract a component dynamically
                let extracted = unsafe { vector_extract_dynamic(vec, index) };
                Vec4::new(extracted, extracted, extracted, extracted)
            }
            1 => {
                // Insert a new value at dynamic index
                unsafe { vector_insert_dynamic(vec, index, 42.0) }
            }
            2 => {
                // Extract and double the value, then insert back
                let extracted = unsafe { vector_extract_dynamic(vec, index) };
                unsafe { vector_insert_dynamic(vec, index, extracted * 2.0) }
            }
            3 => {
                // Swap two components using extract/insert
                let idx1 = index;
                let idx2 = (index + 1) % 4;
                let val1 = unsafe { vector_extract_dynamic(vec, idx1) };
                let val2 = unsafe { vector_extract_dynamic(vec, idx2) };
                let temp = unsafe { vector_insert_dynamic(vec, idx1, val2) };
                unsafe { vector_insert_dynamic(temp, idx2, val1) }
            }
            4 => {
                // Set all components to the value at dynamic index
                let val = unsafe { vector_extract_dynamic(vec, index) };
                Vec4::new(val, val, val, val)
            }
            5 => {
                // Rotate components based on index
                let mut result = vec;
                for i in 0..4 {
                    let src_idx = (i + index) % 4;
                    let val = unsafe { vector_extract_dynamic(vec, src_idx) };
                    result = unsafe { vector_insert_dynamic(result, i, val) };
                }
                result
            }
            6 => {
                // Insert sum of all components at dynamic index
                let sum = vec.x + vec.y + vec.z + vec.w;
                unsafe { vector_insert_dynamic(vec, index, sum) }
            }
            7 => {
                // Extract from one index, insert at another
                let src_idx = index;
                let dst_idx = (index + 2) % 4;
                let val = unsafe { vector_extract_dynamic(vec, src_idx) };
                unsafe { vector_insert_dynamic(vec, dst_idx, val) }
            }
            _ => vec,
        };

        output[tid] = result.to_array();
    }
}
