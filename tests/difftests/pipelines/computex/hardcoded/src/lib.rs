#![no_std]
use spirv_std::spirv;

#[repr(C)]
pub struct Output {
    value: u32,
}

#[spirv(compute(threads(1)))]
pub fn main_cs(#[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut Output) {
    output.value = 42;
}
