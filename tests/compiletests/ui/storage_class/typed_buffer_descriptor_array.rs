// build-pass
// compile-flags: -C target-feature=+RuntimeDescriptorArray,+ext:SPV_EXT_descriptor_indexing

use glam::Vec4;
use spirv_std::spirv;
use spirv_std::{RuntimeArray, TypedBuffer};

#[spirv(fragment)]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] desc_array: &RuntimeArray<
        TypedBuffer<Vec4>,
    >,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] desc_array_mut: &mut RuntimeArray<
        TypedBuffer<Vec4>,
    >,
) {
    unsafe {
        for buffer in 0..3 {
            let mut buffer_from = desc_array.index(buffer);
            let buffer_to = desc_array_mut.index_mut(buffer);
            **buffer_to = **buffer_from;
        }
    }
}
