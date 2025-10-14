// Tests that certain storage class `#[spirv(...)]` attributes are disallowed.

// build-fail
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use spirv_std::spirv;

#[spirv(vertex)]
fn _entry(
    #[spirv(input)] _: (),
    #[spirv(output)] _: (),
    #[spirv(private)] _: (),
    #[spirv(function)] _: (),
    #[spirv(generic)] _: (),
) {
}
