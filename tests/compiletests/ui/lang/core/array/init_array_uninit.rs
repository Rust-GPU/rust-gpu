// Test filling an array with uninitialized values.
// build-pass

use core::mem::MaybeUninit;
use spirv_std::macros::spirv;

#[repr(C)]
pub struct Buf {
    data: [MaybeUninit<u32>; 4],
    len: u32,
}

pub fn clear_field(buf: &mut Buf) {
    buf.data = [const { MaybeUninit::uninit() }; 4];
    buf.len = 0;
}

pub fn clear_whole(data: &mut [MaybeUninit<u32>; 4]) {
    *data = [const { MaybeUninit::uninit() }; 4];
}

#[spirv(fragment)]
pub fn main(o: &mut u32) {
    let mut buf = Buf {
        data: [MaybeUninit::new(*o); 4],
        len: 4,
    };
    clear_field(&mut buf);
    clear_whole(&mut buf.data);
    *o = buf.len;
}
