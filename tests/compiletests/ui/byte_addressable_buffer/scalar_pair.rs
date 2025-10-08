// build-pass

use spirv_std::ByteAddressableBuffer;
use spirv_std::spirv;

#[derive(Copy, Clone, Debug)]
pub struct MyScalarPair(i32, i32);

#[spirv(fragment)]
pub fn load(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &[u32],
    scalar_pair: &mut MyScalarPair,
) {
    unsafe {
        let buf = ByteAddressableBuffer::from_slice(buf);
        *scalar_pair = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn load_mut(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    scalar_pair: &mut MyScalarPair,
) {
    unsafe {
        let buf = ByteAddressableBuffer::from_mut_slice(buf);
        *scalar_pair = buf.load(5);
    }
}

#[spirv(fragment)]
pub fn store(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    #[spirv(flat)] pair0: i32,
    #[spirv(flat)] pair1: i32,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::from_mut_slice(buf);
        buf.store(5, MyScalarPair(pair0, pair1));
    }
}
