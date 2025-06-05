// build-pass

use spirv_std::spirv;

#[repr(C)]
#[derive(Clone, Copy)]
struct Data {
    a: f32,
    b: [f32; 3],
    c: f32,
}

#[repr(C)]
union DataOrArray {
    arr: [f32; 5],
    str: Data,
}

impl DataOrArray {
    fn arr(&self) -> [f32; 5] {
        unsafe { self.arr }
    }
    fn new(arr: [f32; 5]) -> Self {
        Self { arr }
    }
}

#[spirv(fragment)]
pub fn main() {
    let dora = DataOrArray::new([0.0, 0.0, 0.0, 0.0, 0.0]);
    let _arr = dora.arr();
}