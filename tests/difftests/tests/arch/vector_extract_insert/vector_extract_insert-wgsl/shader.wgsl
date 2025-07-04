@group(0) @binding(0) var<storage, read> input: array<vec4<f32>>;
@group(0) @binding(1) var<storage, read> indices: array<u32>;
@group(0) @binding(2) var<storage, read_write> output: array<vec4<f32>>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    
    if (tid < arrayLength(&input) && tid < arrayLength(&indices) && tid < arrayLength(&output)) {
        let vec = input[tid];
        let index = indices[tid] % 4u; // Ensure index is within bounds
        
        // Test various extract and insert operations
        var result: vec4<f32>;
        switch (tid % 8u) {
            case 0u: {
                // Extract a component dynamically
                let extracted = vec[index];
                result = vec4<f32>(extracted, extracted, extracted, extracted);
            }
            case 1u: {
                // Insert a new value at dynamic index
                result = vec;
                result[index] = 42.0;
            }
            case 2u: {
                // Extract and double the value, then insert back
                result = vec;
                result[index] = vec[index] * 2.0;
            }
            case 3u: {
                // Swap two components using extract/insert
                let idx1 = index;
                let idx2 = (index + 1u) % 4u;
                result = vec;
                let temp = result[idx1];
                result[idx1] = result[idx2];
                result[idx2] = temp;
            }
            case 4u: {
                // Set all components to the value at dynamic index
                let val = vec[index];
                result = vec4<f32>(val, val, val, val);
            }
            case 5u: {
                // Rotate components based on index
                for (var i = 0u; i < 4u; i = i + 1u) {
                    let src_idx = (i + index) % 4u;
                    result[i] = vec[src_idx];
                }
            }
            case 6u: {
                // Insert sum of all components at dynamic index
                let sum = vec.x + vec.y + vec.z + vec.w;
                result = vec;
                result[index] = sum;
            }
            case 7u: {
                // Extract from one index, insert at another
                let src_idx = index;
                let dst_idx = (index + 2u) % 4u;
                result = vec;
                result[dst_idx] = vec[src_idx];
            }
            default: {
                result = vec;
            }
        }
        
        output[tid] = result;
    }
}