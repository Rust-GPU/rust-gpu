// build-pass
#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut [u32],
    #[spirv(uniform, descriptor_set = 0, binding = 1)] w: &(u32, u32),
) {
    out[0] = w.0 + w.1;
}
