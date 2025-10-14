// Tests that matrix type inference fails correctly, for empty struct
// build-fail
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use spirv_std::spirv;

#[spirv(matrix)]
pub struct EmptyStruct {}

#[spirv(fragment)]
pub fn entry(#[spirv(push_constant)] matrix: &EmptyStruct) {}
