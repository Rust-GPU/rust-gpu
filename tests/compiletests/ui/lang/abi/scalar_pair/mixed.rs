// build-pass
#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut [u32],
    #[spirv(uniform, descriptor_set = 0, binding = 1)] w: &(u32, f32),
) {
    let a = w.0;
    let b_bits = w.1.to_bits();
    out[0] = a ^ b_bits;
}
