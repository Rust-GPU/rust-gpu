#![no_std]

use spirv_std::glam::UVec3;
use spirv_std::spirv;

#[spirv(compute(threads(8, 4, 2)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut [u32],
    #[spirv(workgroup_size)] workgroup_size: UVec3,
    #[spirv(global_invocation_id)] global_id: UVec3,
    #[spirv(local_invocation_id)] local_id: UVec3,
) {
    let idx = global_id.x as usize;

    if idx < output.len() {
        // Store a value that encodes the workgroup dimensions
        // This allows us to verify that the workgroup_size builtin is working correctly
        let encoded = (workgroup_size.x << 16) | (workgroup_size.y << 8) | workgroup_size.z;

        // Also encode the local invocation ID to show it's within the workgroup bounds
        let local_encoded = (local_id.x << 16) | (local_id.y << 8) | local_id.z;

        // Store: encoded workgroup size in even indices, local ID in odd indices
        if idx % 2 == 0 {
            output[idx] = encoded; // Should be (8 << 16) | (4 << 8) | 2 = 0x080402
        } else {
            output[idx] = local_encoded;
        }
    }
}
