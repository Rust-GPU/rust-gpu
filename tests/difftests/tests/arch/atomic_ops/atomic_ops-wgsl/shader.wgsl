@group(0) @binding(0)
var<storage, read_write> counters: array<atomic<u32>, 5>;

@group(0) @binding(1)
var<storage, read_write> output: array<u32>;

@compute @workgroup_size(32, 1, 1)
fn main_cs(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    
    // All threads participate in atomic operations
    // Each thread adds 1 to the first counter
    atomicAdd(&counters[0], 1u);
    
    // Each thread subtracts 1 from the second counter
    atomicSub(&counters[1], 1u);
    
    // Each thread tries to set minimum with their thread ID
    atomicMin(&counters[2], tid);
    
    // Each thread tries to set maximum with their thread ID
    atomicMax(&counters[3], tid);
    
    // Thread 0 stores the final values after all operations complete
    if (tid == 0u) {
        // Synchronize to ensure all atomic operations are complete
        workgroupBarrier();
        output[0] = atomicLoad(&counters[0]); // Should be initial + 32
        output[1] = atomicLoad(&counters[1]); // Should be initial - 32
        output[2] = atomicLoad(&counters[2]); // Should be min(initial, 0)
        output[3] = atomicLoad(&counters[3]); // Should be max(initial, 31)
        output[4] = atomicLoad(&counters[4]); // Unchanged
    }
}