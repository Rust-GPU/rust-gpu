// build-pass
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-vulkan1.0

use core::ops::{Add, AddAssign};
use spirv_std::arch::workgroup_memory_barrier_with_group_sync;
use spirv_std::glam::*;
use spirv_std::spirv;

#[derive(Copy, Clone, Debug)]
pub struct Nested(pub i32);

impl Add for Nested {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl AddAssign for Nested {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Value {
    pub a: f32,
    pub b: UVec4,
    pub c: Nested,
    pub d: Mat4,
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            a: self.a + rhs.a,
            b: self.b + rhs.b,
            c: self.c + rhs.c,
            d: self.d + rhs.d,
        }
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
