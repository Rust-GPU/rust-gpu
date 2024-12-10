// build-pass

use spirv_std::ByteAddressableBuffer;
use spirv_std::spirv;

pub struct BigStruct {
    a: u32,
    b: u32,
    c: u32,
    d: u32,
    e: u32,
    f: u32,
}

#[spirv(fragment)]
pub fn load(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &[u32],
    out: &mut BigStruct,
) {
    unsafe {
        let buf = ByteAddressableBuffer::from_slice(buf);
        *out = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn load_mut(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    out: &mut BigStruct,
) {
    unsafe {
        let buf = ByteAddressableBuffer::from_mut_slice(buf);
        *out = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn store(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    #[spirv(flat)] val: BigStruct,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::from_mut_slice(buf);
        buf.store(5, val);
    }
}
