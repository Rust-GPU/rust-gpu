// Doesn't work, only worked before because I think it got optimized away before
// hitting the backend.

// build-fail

#![feature(ptr_internals)]

use spirv_std::spirv;

use core::ptr::Unique;

const POINTER: Unique<()> = Unique::<()>::dangling();

#[spirv(fragment)]
pub fn main(output: &mut Unique<()>) {
    *output = POINTER;
}
