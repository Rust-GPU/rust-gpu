// build-pass
// compile-flags: -C target-feature=+Int64
#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut [u32],
    #[spirv(uniform, descriptor_set = 0, binding = 1)] w: &(i32, i32),
) {
    // Sum and reinterpret as u32 for output
    let s = (w.0 as i64 + w.1 as i64) as i32;
    out[0] = s as u32;
}
