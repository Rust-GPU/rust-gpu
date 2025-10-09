// build-pass

use spirv_std::arch::workgroup_memory_barrier_with_group_sync;
use spirv_std::glam::*;
use spirv_std::spirv;

pub type Value = i32;

pub const WG_SIZE_SHIFT: usize = 5;
pub const WG_SIZE: usize = 1 << WG_SIZE_SHIFT;

// threads must be a literal, constants don't work
const _: () = {
    assert!(WG_SIZE == 32);
};
#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] input: &[Value],
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] output: &mut Value,
    #[spirv(workgroup)] shared: &mut [Value; WG_SIZE],
    #[spirv(local_invocation_index)] inv_id: UVec3,
) {
    unsafe {
        let inv_id = inv_id.x as usize;
        shared[inv_id] = input[inv_id];
        workgroup_memory_barrier_with_group_sync();

        let mut mask = WG_SIZE << 1;
        while mask != 0 {
            if inv_id < mask {
                shared[inv_id] += shared[inv_id + mask];
            }
            workgroup_memory_barrier_with_group_sync();
            mask <<= 1;
        }

        if inv_id == 0 {
            *output = shared[0];
        }
    }
}
