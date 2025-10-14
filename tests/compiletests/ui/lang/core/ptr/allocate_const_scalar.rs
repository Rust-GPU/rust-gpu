// Doesn't work, only worked before because I think it got optimized away before
// hitting the backend.

// build-fail
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

#![feature(ptr_internals)]

use spirv_std::spirv;

use core::ptr::Unique;

const POINTER: Unique<()> = Unique::<()>::dangling();

#[spirv(fragment)]
pub fn main(output: &mut Unique<()>) {
    *output = POINTER;
}
