#![no_std]

use spirv_std::arch::workgroup_memory_barrier_with_group_sync;
use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(workgroup)] shared: &mut [u32; 64],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
    #[spirv(local_invocation_id)] local_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;
    let lid = local_id.x as usize;

    if tid < input.len() && tid < output.len() && lid < 64 {
        // Load data into shared memory
        shared[lid] = input[tid];

        // Workgroup barrier to ensure all threads have loaded their data
        workgroup_memory_barrier_with_group_sync();

        // Perform operations on shared memory
        let mut result = shared[lid];

        // Different threads perform different operations
        match lid % 4 {
            0 => {
                // Read from neighboring thread's data
                if lid + 1 < 64 {
                    result += shared[lid + 1];
                }
            }
            1 => {
                // Read from previous thread's data
                if lid > 0 {
                    result += shared[lid - 1];
                }
            }
            2 => {
                // Sum reduction within groups of 4
                if lid + 2 < 64 {
                    result = shared[lid] + shared[lid + 1] + shared[lid + 2];
                }
            }
            _ => {
                // XOR with wrapped neighbor
                result ^= shared[(lid + 32) % 64];
            }
        }

        // Another barrier before writing back
        workgroup_memory_barrier_with_group_sync();

        // Write result back to shared memory
        shared[lid] = result;

        // Memory barrier to ensure writes are visible
        workgroup_memory_barrier_with_group_sync();

        // Final read and output
        output[tid] = shared[lid];
    }
}
