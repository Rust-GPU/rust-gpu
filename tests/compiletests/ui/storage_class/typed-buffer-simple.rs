// build-pass
// compile-flags: -C target-feature=+RuntimeDescriptorArray,+ext:SPV_EXT_descriptor_indexing

// Tests the simplest `TypedBuffer` case: Multiple structs of of the same type and size.

use spirv_std::{RuntimeArray, TypedBuffer, glam::UVec3, spirv};

#[derive(Clone, Copy)]
pub struct MyData {
    some_big_data: [u32; 1 << 24],
}

#[spirv(compute(threads(1)))]
pub fn compute(
    #[spirv(global_invocation_id)] global_invocation_id: UVec3,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] my_data: &mut RuntimeArray<
        TypedBuffer<MyData>,
    >,
) {
    let mut load_dta = unsafe { my_data.index(global_invocation_id.x as usize) }.some_big_data[0];
    load_dta = 32;

    let mut target = unsafe { &mut my_data.index_mut(global_invocation_id.y as usize) };
    target.some_big_data[0] = load_dta;
}
