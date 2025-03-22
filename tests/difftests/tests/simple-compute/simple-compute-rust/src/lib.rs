#![cfg(target_arch = "spirv")]
#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main_cs(#[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut [u32]) {
    output[0] = 42;
}
