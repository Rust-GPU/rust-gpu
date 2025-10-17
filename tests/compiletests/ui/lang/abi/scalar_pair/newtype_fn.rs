// build-pass
#![no_std]

use spirv_std::spirv;

#[repr(transparent)]
pub struct Inner((u32, u32));

#[repr(transparent)]
pub struct Outer(
    core::mem::ManuallyDrop<Inner>,
    core::marker::PhantomData<()>,
);

#[inline(never)]
fn sum_outer(o: Outer) -> u32 {
    // SAFETY: repr(transparent) guarantees same layout as `Inner`.
    let i: Inner = unsafe { core::mem::ManuallyDrop::into_inner((o.0)) };
    (i.0).0 + (i.0).1
}

#[spirv(compute(threads(1)))]
pub fn main(#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] out: &mut [u32]) {
    let i = Inner((5, 7));
    let o = Outer(core::mem::ManuallyDrop::new(i), core::marker::PhantomData);
    out[0] = sum_outer(o);
}
