// build-pass
// compile-flags: -C target-feature=+Int64
#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut [u32],
    #[spirv(uniform, descriptor_set = 0, binding = 1)] w: &(u64, u32),
) {
    // Fold 64-bit into 32-bit deterministically
    let hi = (w.0 >> 32) as u32;
    let lo = (w.0 & 0xFFFF_FFFF) as u32;
    out[0] = hi ^ lo ^ w.1;
}
