@group(0) @binding(0)
var<storage, read> input: array<u32>;

@group(0) @binding(1)
var<storage, read_write> output: array<u32>;

@compute @workgroup_size(64, 1, 1)
fn main_cs(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    let input_len = arrayLength(&input);
    
    // Test various array access patterns
    
    // 1. Direct indexing
    if (tid < input_len) {
        output[tid] = input[tid] * 2u;
    }
    
    // 2. Strided access (every 4th element)
    let stride_idx = tid * 4u;
    if (stride_idx < input_len && tid + 64u < arrayLength(&output)) {
        output[tid + 64u] = input[stride_idx];
    }
    
    // 3. Reverse indexing
    if (tid < input_len && tid + 128u < arrayLength(&output)) {
        let reverse_idx = input_len - 1u - tid;
        output[tid + 128u] = input[reverse_idx];
    }
    
    // 4. Gather operation (indirect indexing)
    if (tid < 16u && tid + 192u < arrayLength(&output)) {
        // Use first 16 values as indices
        let index = input[tid];
        if (index < input_len) {
            output[tid + 192u] = input[index];
        }
    }
    
    // 5. Write pattern to test bounds
    if (tid == 0u) {
        // Write sentinel values at specific positions
        output[208] = 0xDEADBEEFu;
        output[209] = 0xCAFEBABEu;
        output[210] = input_len;
    }
}