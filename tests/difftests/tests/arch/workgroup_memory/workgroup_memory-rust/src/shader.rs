use spirv_std::arch::workgroup_memory_barrier_with_group_sync;
use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(local_invocation_id)] local_id: spirv_std::glam::UVec3,
    #[spirv(workgroup)] shared: &mut [u32; 64],
) {
    let lid = local_id.x as usize;

    // Load data into shared memory
    shared[lid] = input[lid];

    // Synchronize to ensure all threads have loaded
    workgroup_memory_barrier_with_group_sync();

    // Each thread sums its value with its neighbor (reduction step)
    if lid < 32 {
        shared[lid] += shared[lid + 32];
    }

    // Synchronize again
    workgroup_memory_barrier_with_group_sync();

    if lid < 16 {
        shared[lid] += shared[lid + 16];
    }

    workgroup_memory_barrier_with_group_sync();

    if lid < 8 {
        shared[lid] += shared[lid + 8];
    }

    workgroup_memory_barrier_with_group_sync();

    if lid < 4 {
        shared[lid] += shared[lid + 4];
    }

    workgroup_memory_barrier_with_group_sync();

    if lid < 2 {
        shared[lid] += shared[lid + 2];
    }

    workgroup_memory_barrier_with_group_sync();

    if lid < 1 {
        shared[lid] += shared[lid + 1];
    }

    workgroup_memory_barrier_with_group_sync();

    // Write final result
    if lid == 0 {
        output[0] = shared[0];
    }
}
