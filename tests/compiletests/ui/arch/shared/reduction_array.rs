// build-pass
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-vulkan1.0

use core::ops::{Add, AddAssign, Deref, DerefMut};
use spirv_std::arch::workgroup_memory_barrier_with_group_sync;
use spirv_std::glam::*;
use spirv_std::spirv;

#[derive(Copy, Clone, Debug)]
pub struct Value(pub [f32; 4]);

impl Deref for Value {
    type Target = [f32; 4];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Value {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self([
            self[0] + rhs[0],
            self[1] + rhs[1],
            self[2] + rhs[2],
            self[3] + rhs[3],
        ])
    }
}

impl AddAssign for Value {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

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
    #[spirv(local_invocation_index)] inv_id: u32,
) {
    unsafe {
        let inv_id = inv_id as usize;
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
