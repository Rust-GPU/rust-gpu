@group(0) @binding(0)
var<storage, read_write> output: array<u32>;

// Define workgroup dimensions as named constants
const WORKGROUP_SIZE_X: u32 = 8u;
const WORKGROUP_SIZE_Y: u32 = 4u;
const WORKGROUP_SIZE_Z: u32 = 2u;

@compute @workgroup_size(WORKGROUP_SIZE_X, WORKGROUP_SIZE_Y, WORKGROUP_SIZE_Z)
fn main_cs(
    @builtin(global_invocation_id) global_id: vec3<u32>,
    @builtin(local_invocation_id) local_id: vec3<u32>
) {
    let idx = global_id.x;
    
    if (idx < arrayLength(&output)) {
        // Use the named constants to create the workgroup_size vector
        let workgroup_size = vec3<u32>(WORKGROUP_SIZE_X, WORKGROUP_SIZE_Y, WORKGROUP_SIZE_Z);
        
        // Store a value that encodes the workgroup dimensions
        let encoded = (workgroup_size.x << 16u) | (workgroup_size.y << 8u) | workgroup_size.z;
        
        // Also encode the local invocation ID
        let local_encoded = (local_id.x << 16u) | (local_id.y << 8u) | local_id.z;
        
        // Store: encoded workgroup size in even indices, local ID in odd indices
        if (idx % 2u == 0u) {
            output[idx] = encoded; // Should be (8 << 16) | (4 << 8) | 2 = 0x080402
        } else {
            output[idx] = local_encoded;
        }
    }
}
