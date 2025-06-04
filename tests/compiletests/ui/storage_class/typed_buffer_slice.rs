// build-pass

use glam::Vec4;
use spirv_std::TypedBuffer;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] slice: &TypedBuffer<[Vec4]>,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] slice_mut: &mut TypedBuffer<[Vec4]>,
) {
    for i in 0..5 {
        slice_mut[i] = slice[i];
    }
}
