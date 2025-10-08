// build-pass

use spirv_std::ByteAddressableBuffer;
use spirv_std::spirv;

#[derive(Copy, Clone, Debug)]
pub struct MyScalar(i32);

#[spirv(fragment)]
pub fn load_scalar(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &[u32],
    scalar: &mut MyScalar,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::from_slice(buf);
        *scalar = buf.load(5);
    }
}

#[derive(Copy, Clone, Debug)]
pub struct MyScalarPair(i32, i32);

#[spirv(fragment)]
pub fn load_scalar_pair(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &[u32],
    scalar_pair: &mut MyScalarPair,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::from_slice(buf);
        *scalar_pair = buf.load(5);
    }
}
