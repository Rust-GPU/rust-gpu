#![no_std]
use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] data: &mut [u32],
) {
    data[0] = 42;
}
