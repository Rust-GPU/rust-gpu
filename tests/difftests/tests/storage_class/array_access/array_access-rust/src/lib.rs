#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    // Test various array access patterns

    // 1. Direct indexing
    if tid < input.len() {
        output[tid] = input[tid] * 2;
    }

    // 2. Strided access (every 4th element)
    let stride_idx = tid * 4;
    if stride_idx < input.len() && tid + 64 < output.len() {
        output[tid + 64] = input[stride_idx];
    }

    // 3. Reverse indexing
    if tid < input.len() && tid + 128 < output.len() {
        let reverse_idx = input.len() - 1 - tid;
        output[tid + 128] = input[reverse_idx];
    }

    // 4. Gather operation (indirect indexing)
    if tid < 16 && tid + 192 < output.len() {
        // Use first 16 values as indices
        let index = input[tid] as usize;
        if index < input.len() {
            output[tid + 192] = input[index];
        }
    }

    // 5. Write pattern to test bounds
    if tid == 0 {
        // Write sentinel values at specific positions
        output[208] = 0xDEADBEEF;
        output[209] = 0xCAFEBABE;
        output[210] = input.len() as u32;
    }
}
