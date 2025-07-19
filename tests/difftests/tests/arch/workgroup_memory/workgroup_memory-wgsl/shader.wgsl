@group(0) @binding(0)
var<storage, read> input: array<u32>;

@group(0) @binding(1)
var<storage, read_write> output: array<u32>;

var<workgroup> shared_data: array<u32, 64>;

@compute @workgroup_size(64, 1, 1)
fn main_cs(@builtin(local_invocation_id) local_id: vec3<u32>) {
    let lid = local_id.x;
    
    // Load data into shared memory
    shared_data[lid] = input[lid];
    
    // Synchronize to ensure all threads have loaded
    workgroupBarrier();
    
    // Each thread sums its value with its neighbor (reduction step)
    if (lid < 32u) {
        shared_data[lid] += shared_data[lid + 32u];
    }
    
    // Synchronize again
    workgroupBarrier();
    
    if (lid < 16u) {
        shared_data[lid] += shared_data[lid + 16u];
    }
    
    workgroupBarrier();
    
    if (lid < 8u) {
        shared_data[lid] += shared_data[lid + 8u];
    }
    
    workgroupBarrier();
    
    if (lid < 4u) {
        shared_data[lid] += shared_data[lid + 4u];
    }
    
    workgroupBarrier();
    
    if (lid < 2u) {
        shared_data[lid] += shared_data[lid + 2u];
    }
    
    workgroupBarrier();
    
    if (lid < 1u) {
        shared_data[lid] += shared_data[lid + 1u];
    }
    
    workgroupBarrier();
    
    // Write final result
    if (lid == 0u) {
        output[0] = shared_data[0];
    }
}