// Test that propagating `#[track_caller]` doesn't cause constant-related errors.

// build-pass
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use spirv_std::spirv;

#[track_caller]
fn track_caller_maybe_panic(x: u32) {
    if x > 0 {
        panic!();
    }
}

#[spirv(fragment)]
pub fn main(#[spirv(flat)] x: u32) {
    track_caller_maybe_panic(x);
}
