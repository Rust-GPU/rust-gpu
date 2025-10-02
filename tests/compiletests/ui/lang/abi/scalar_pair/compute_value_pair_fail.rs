// compile-fail
#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    w: (u32, u32),
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut [u32],
) {
    out[0] = w.0 + w.1;
}
