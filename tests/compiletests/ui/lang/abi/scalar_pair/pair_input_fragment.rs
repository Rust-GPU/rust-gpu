// build-pass
#![no_std]

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(flat)] pi: (u32, u32),
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] out: &mut [u32],
) {
    out[0] = pi.0.wrapping_add(pi.1);
}
