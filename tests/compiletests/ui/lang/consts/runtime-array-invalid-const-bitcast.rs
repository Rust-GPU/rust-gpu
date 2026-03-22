// build-fail

use spirv_std::{RuntimeArray, spirv};

// Reinterpreting a 3-byte backing allocation as `RuntimeArray<u16>` leaves
// one trailing byte that cannot form a whole element.
const BYTES: &[u8; 3] = &[1, 2, 3];
const BAD_RUNTIME_ARRAY_PTR: *const RuntimeArray<u16> =
    BYTES as *const [u8; 3] as *const RuntimeArray<u16>;

#[inline(never)]
fn use_runtime_array_ptr(ptr: *const RuntimeArray<u16>) -> bool {
    !ptr.is_null()
}

#[spirv(fragment)]
pub fn main(output: &mut u32) {
    *output = u32::from(use_runtime_array_ptr(BAD_RUNTIME_ARRAY_PTR));
}
