// build-pass

use glam::Vec4;
use spirv_std::TypedBuffer;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] single: &TypedBuffer<Vec4>,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] single_mut: &mut TypedBuffer<Vec4>,
) {
    **single_mut = **single;
}
