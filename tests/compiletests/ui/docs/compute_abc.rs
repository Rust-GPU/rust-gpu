// built-pass

use spirv_std::glam::*;
use spirv_std::spirv;

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &[u32],
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &[u32],
    #[spirv(descriptor_set = 0, binding = 2, storage_buffer)] c: &mut [u32],
    #[spirv(global_invocation_id)] global_index: UVec3,
) {
    let i = global_index.x as usize;
    c[i] = a[i] + b[i];
}
