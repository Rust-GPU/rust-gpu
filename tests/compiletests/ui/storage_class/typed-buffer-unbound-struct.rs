// build-pass
// compile-flags: -C target-feature=+RuntimeDescriptorArray,+ext:SPV_EXT_descriptor_indexing

// Tests the more complex `TypedBuffer` case, where the size of each buffer in the binding is unbound, and also the data type is a struct.

use spirv_std::{RuntimeArray, TypedBuffer, glam::UVec3, spirv};

#[derive(Clone, Copy)]
pub struct MyData {
    a: f32,
    b: [u32; 3],
}

#[spirv(compute(threads(1)))]
pub fn compute(
    #[spirv(global_invocation_id)] global_invocation_id: UVec3,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] my_data: &mut RuntimeArray<
        TypedBuffer<[MyData]>,
    >,
) {
    let mut load_dta: MyData = unsafe { my_data.index(global_invocation_id.x as usize) }[0];
    load_dta.b[0] = 32;

    let mut target = unsafe { &mut my_data.index_mut(global_invocation_id.y as usize)[0] };
    *target = load_dta;
}
