@group(0) @binding(0)
var<storage, read> input: array<u32>;

@group(0) @binding(1)
var<storage, read_write> output: array<u32>;

var<workgroup> shared_mem: array<u32, 64>;

@compute @workgroup_size(64)
fn main(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(local_invocation_id) local_id: vec3<u32>
) {
    let tid = global_id.x;
    let lid = local_id.x;
    
    if (tid < arrayLength(&input) && tid < arrayLength(&output) && lid < 64u) {
        // Load data into shared memory
        shared_mem[lid] = input[tid];
        
        // Workgroup barrier to ensure all threads have loaded their data
        workgroupBarrier();
        
        // Perform operations on shared memory
        var result = shared_mem[lid];
        
        // Different threads perform different operations
        if (lid % 4u == 0u) {
            // Read from neighboring thread's data
            if (lid + 1u < 64u) {
                result = result + shared_mem[lid + 1u];
            }
        } else if (lid % 4u == 1u) {
            // Read from previous thread's data
            if (lid > 0u) {
                result = result + shared_mem[lid - 1u];
            }
        } else if (lid % 4u == 2u) {
            // Sum reduction within groups of 4
            if (lid + 2u < 64u) {
                result = shared_mem[lid] + shared_mem[lid + 1u] + shared_mem[lid + 2u];
            }
        } else {
            // XOR with wrapped neighbor
            result = result ^ shared_mem[(lid + 32u) % 64u];
        }
        
        // Another barrier before writing back
        workgroupBarrier();
        
        // Write result back to shared memory
        shared_mem[lid] = result;
        
        // Memory barrier to ensure writes are visible
        workgroupBarrier();
        
        // Final read and output
        output[tid] = shared_mem[lid];
    }
}