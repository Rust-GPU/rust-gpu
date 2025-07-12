#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main_cs(#[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut [u32]) {
    let hash = blake3::Hasher::new().finalize();
    let bytes = hash.as_bytes();

    for i in 0..8 {
        output[i] = u32::from_le_bytes([bytes[i], bytes[i + 1], bytes[i + 2], bytes[i + 3]]);
    }
}
