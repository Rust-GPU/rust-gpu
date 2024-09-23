// build-pass

use spirv_std::spirv;
use spirv_std::{ByteAddressableBuffer, MutByteAddressableBuffer};

#[spirv(fragment)]
pub fn load(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    out: &mut f32,
) {
    unsafe {
        let buf = ByteAddressableBuffer::new(buf);
        *out = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn store(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32], val: f32) {
    unsafe {
        let mut buf = MutByteAddressableBuffer::new(buf);
        buf.store(5, val);
    }
}
