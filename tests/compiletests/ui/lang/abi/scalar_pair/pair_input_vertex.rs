// build-pass
#![no_std]

use spirv_std::spirv;

#[spirv(vertex)]
pub fn main(
    p: (u32, u32),
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] out: &mut [u32],
) {
    out[0] = p.0.wrapping_add(p.1);
}
