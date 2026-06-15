// build-pass
// compile-flags: -C target-feature=+Float64,+Int8,+Int16

use spirv_std::glam::Vec4;
use spirv_std::spirv;

#[repr(C)]
#[derive(Clone, Copy)]
struct Data {
    a: i32,
    b: [f32; 3],
    c: u32,
}

union DataOrArray {
    arr: [f32; 5],
    str: Data,
}

impl DataOrArray {
    fn arr(self) -> [f32; 5] {
        unsafe { self.arr }
    }
    fn new(arr: [f32; 5]) -> Self {
        Self { arr }
    }
}

#[spirv(fragment)]
pub fn union_mixed_types() {
    let dora = DataOrArray::new([1.0, 2.0, 3.0, 4.0, 5.0]);
    let arr = dora.arr();
}

union TwoU8OrU16 {
    bytes: [u8; 2],
    half: u16,
}

#[spirv(fragment)]
pub fn union_two_u8_as_u16(output: &mut u16) {
    let u = TwoU8OrU16 {
        bytes: [0xABu8, 0xCDu8],
    };
    *output = unsafe { u.half };
}

// ── N:M groups: [u8; 4] -> [u16; 2]
// Group 1: u8(0)+u8(1) = u16(0)
// Group 2: u8(2)+u8(3) = u16(1)

union FourU8OrTwoU16 {
    bytes: [u8; 4],
    halves: [u16; 2],
}

#[spirv(fragment)]
pub fn fragment_union_four_u8_as_two_u16(output: &mut [u16; 2]) {
    let u = FourU8OrTwoU16 {
        bytes: [1u8, 2u8, 3u8, 4u8],
    };
    *output = unsafe { u.halves };
}

#[spirv(fragment)]
pub fn fragment_union_two_u16_as_four_u8(output: &mut [u8; 4]) {
    let u = FourU8OrTwoU16 {
        halves: [0x0102u16, 0x0304u16],
    };
    *output = unsafe { u.bytes };
}

// [f32; 2]  -> f64 (64 bits)
union TwoF32OrF64 {
    pair: [f32; 2],
    wide: f64,
}

#[spirv(fragment)]
pub fn fragment_union_two_f32_as_f64(output: &mut f64) {
    let u = TwoF32OrF64 {
        pair: [1.0f32, 2.0f32],
    };
    *output = unsafe { u.wide };
}

//  f64  -> [f32; 2]

#[spirv(fragment)]
pub fn fragment_union_f64_as_two_f32(output: &mut [f32; 2]) {
    let u = TwoF32OrF64 { wide: 0.0f64 };
    *output = unsafe { u.pair };
}

//  struct -> array, same total size, no padding
// struct { x: u32, y: u32, z: u32 } (12 bytes) -> [f32; 3] (12 bytes).

#[repr(C)]
#[derive(Clone, Copy)]
pub struct ThreeU32 {
    pub x: u32,
    pub y: u32,
    pub z: u32,
}

union ThreeU32OrF32Array {
    s: ThreeU32,
    a: [f32; 3],
}

#[spirv(fragment)]
pub fn fragment_union_struct_u32_vs_f32_array(output: &mut [f32; 3]) {
    let u = ThreeU32OrF32Array {
        s: ThreeU32 { x: 0, y: 3, z: 2 },
    };
    *output = unsafe { u.a };
}

// ── 1:1, struct -> array, four f32 fields (16 bytes, no padding)

#[repr(C)]
#[derive(Clone, Copy)]
pub struct MyVec4 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub w: f32,
}

union ArrayOrMyVec4 {
    arr: [f32; 4],
    vec: MyVec4,
}

#[spirv(fragment)]
pub fn union_array_vs_struct_vec4(output: &mut MyVec4) {
    let u = ArrayOrMyVec4 {
        arr: [1.0, 2.0, 3.0, 4.0],
    };
    *output = unsafe { u.vec };
}

union FourF32OrTwoF64 {
    narrow: [f32; 4],
    wide: [f64; 2],
}

#[spirv(fragment)]
pub fn union_four_f32_as_two_f64(output: &mut [f64; 2]) {
    let u = FourF32OrTwoF64 {
        narrow: [1.0f32, 2.0f32, 3.0f32, 4.0f32],
    };
    *output = unsafe { u.wide };
}

#[spirv(fragment)]
pub fn union_two_f64_as_four_f32(output: &mut [f32; 4]) {
    let u = FourF32OrTwoF64 {
        wide: [0.0f64, 1.0f64],
    };
    *output = unsafe { u.narrow };
}
