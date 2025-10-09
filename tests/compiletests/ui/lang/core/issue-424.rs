// build-pass

#![no_std]

use core::marker::PhantomData;
use spirv_std::spirv;

pub struct BitSlice<T, O> {
    _ord: PhantomData<O>,
    _typ: PhantomData<[T]>,
    _mem: [()],
}

#[spirv(compute(threads(1)))]
pub fn issue424() {}
