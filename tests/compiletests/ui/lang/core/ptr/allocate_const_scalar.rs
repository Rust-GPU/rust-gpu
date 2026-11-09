// Doesn't work, only worked before because I think it got optimized away before
// hitting the backend.

// build-fail

use spirv_std::spirv;

use core::ptr::NonNull;

const POINTER: NonNull<()> = NonNull::<()>::dangling();

#[spirv(fragment)]
pub fn main(output: &mut NonNull<()>) {
    *output = POINTER;
}
