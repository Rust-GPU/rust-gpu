// build-pass

#![feature(const_generics)]
#![allow(incomplete_features)]

use spirv_std::{memory::{Scope, Semantics}, storage_class::Image};

#[spirv(fragment)]
pub fn main(mut output: Image<f32>) {
    unsafe {
        spirv_std::arch::atomic_store::<
            _,
            { Scope::CrossDevice },
            { Semantics::ImageMemory }
        >(&mut *output, 5.0);
    }
}
