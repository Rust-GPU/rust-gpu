// build-pass

use spirv_std::ByteAddressableBuffer;
use spirv_std::spirv;

#[derive(Copy, Clone, Debug)]
pub struct MyScalar(i32);

#[spirv(fragment)]
pub fn store_scalar(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    #[spirv(flat)] scalar: MyScalar,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::from_mut_slice(buf);
        buf.store(5, scalar);
    }
}

#[derive(Copy, Clone, Debug)]
pub struct MyScalarPair(i32, i32);

#[spirv(fragment)]
pub fn store_scalar_pair(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut [u32],
    #[spirv(flat)] pair0: i32,
    #[spirv(flat)] pair1: i32,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::from_mut_slice(buf);
        buf.store(5, MyScalarPair(pair0, pair1));
    }
}
