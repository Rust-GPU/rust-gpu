// Test that u32::from(u64) fails to compile since From<u64> is not implemented for u32
// This ensures our From trait optimization doesn't accidentally allow invalid conversions

// build-fail
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use spirv_std::spirv;

const K: u64 = 42;

#[spirv(fragment)]
pub fn main(output: &mut u32) {
    // This should fail to compile because From<u64> is not implemented for u32
    // (u64 to u32 is a narrowing conversion that could lose data)
    let value = u32::from(K);
    *output = value;
}
