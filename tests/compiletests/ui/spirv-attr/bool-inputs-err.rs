// build-fail
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use spirv_std::spirv;

pub struct Boolthing {
    x: u32,
    y: u32,
    b: bool,
}

#[spirv(fragment)]
pub fn fragment(
    input: bool,
    output: &mut bool,
    #[spirv(push_constant)] push: &bool,
    #[spirv(uniform)] uniform: &Boolthing,
) {
}
