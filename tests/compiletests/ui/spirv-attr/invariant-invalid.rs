// Tests that the invariant attribute can't be applied on inputs
// build-fail
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use spirv_std::spirv;

#[spirv(vertex)]
pub fn main(#[spirv(invariant)] input: f32) {}
