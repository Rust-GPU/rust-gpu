// build-pass

use spirv_std::spirv;

#[derive(Default)]
struct Foo {
    bar: bool,
    baz: [[u32; 2]; 1],
}

#[spirv(fragment)]
pub fn main() {
    let x = [[1; 2]; 1];
    let y = [Foo::default(); 1];
}
