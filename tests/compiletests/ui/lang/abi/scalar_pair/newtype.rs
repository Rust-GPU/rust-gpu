// build-pass
#![no_std]

use spirv_std::spirv;

#[repr(transparent)]
pub struct Wrap((u32, u32));

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut [u32],
    #[spirv(uniform, descriptor_set = 0, binding = 1)] w: &Wrap,
) {
    let a = (w.0).0;
    let b = (w.0).1;
    out[0] = a + b;
}
