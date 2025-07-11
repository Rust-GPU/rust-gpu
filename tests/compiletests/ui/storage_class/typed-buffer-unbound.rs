// build-pass
// compile-flags: -C target-feature=+RuntimeDescriptorArray,+ext:SPV_EXT_descriptor_indexing

// Tests the more complex `TypedBuffer` case, where the size of each buffer in the binding is unbound.

use spirv_std::{RuntimeArray, TypedBuffer, glam::UVec3, spirv};

#[spirv(compute(threads(1)))]
pub fn compute(
    #[spirv(global_invocation_id)] global_invocation_id: UVec3,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] my_data: &mut RuntimeArray<
        TypedBuffer<[u32]>,
    >,
) {
    let mut load_dta: u32 = unsafe { my_data.index(global_invocation_id.x as usize) }[0];
    load_dta = 32;

    let mut target = unsafe { &mut my_data.index_mut(global_invocation_id.y as usize)[0] };
    *target = load_dta;
}
