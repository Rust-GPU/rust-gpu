#![no_std]

use spirv_std::spirv;

// Non-zero, so a fill that writes nothing is visible
pub const INPUT: [u32; 4] = [1, 2, 3, 4];

pub const ROWS: usize = 9;
pub const WORDS: usize = 6;
// One row per fill, the contents of the aggregate after it
pub type Output = [[u32; WORDS]; ROWS];

// `repr(C)` keeps the array at offset 0
#[repr(C)]
#[derive(Clone, Copy)]
pub struct First {
    pub arr: [u32; 4],
    pub tail: u32,
}

#[repr(C)]
pub struct Nested {
    pub head: u32,
    pub first: First,
}

pub struct Only {
    pub arr: [u32; 4],
}

pub fn reset(s: &mut First) {
    s.arr = [0; 4];
}

// Borrowed, so the struct is not split into separate variables
fn first_words(s: &First) -> [u32; WORDS] {
    [s.arr[0], s.arr[1], s.arr[2], s.arr[3], s.tail, 0]
}

fn nested_words(n: &Nested) -> [u32; WORDS] {
    let [a0, a1, a2, a3] = n.first.arr;
    [n.head, a0, a1, a2, a3, n.first.tail]
}

fn only_words(s: &Only) -> [u32; WORDS] {
    [s.arr[0], s.arr[1], s.arr[2], s.arr[3], 0, 0]
}

fn table_words(t: &[[u32; 2]; 3]) -> [u32; WORDS] {
    [t[0][0], t[0][1], t[1][0], t[1][1], t[2][0], t[2][1]]
}

pub fn eval(input: &[u32; 4]) -> Output {
    let [a, b, c, d] = *input;
    let first = First {
        arr: [a, b, c, d],
        tail: a,
    };
    let table = [[a, b], [c, d], [a, d]];

    let mut field = first;
    field.arr = [0; 4];

    let mut param = first;
    reset(&mut param);

    // The first field of a struct that is itself at a non-zero offset
    let mut nested = Nested { head: b, first };
    nested.first.arr = [u32::MAX; 4];

    let mut elem0 = table;
    elem0[0] = [0; 2];

    let mut reborrow = table;
    let row = &mut reborrow[0];
    *row = [0; 2];

    let mut floats = [[a as f32, b as f32], [c as f32, d as f32]];
    floats[0] = [0.0; 2];

    // For comparison: a non-zero offset, an array that is the only field, and
    // a fill that is not lowered to `memset`
    let mut elem1 = table;
    elem1[1] = [0; 2];

    let mut only = Only { arr: [a, b, c, d] };
    only.arr = [0; 4];

    let mut ones = first;
    ones.arr = [1; 4];

    [
        first_words(&field),
        first_words(&param),
        nested_words(&nested),
        table_words(&elem0),
        table_words(&reborrow),
        [
            floats[0][0].to_bits(),
            floats[0][1].to_bits(),
            floats[1][0].to_bits(),
            floats[1][1].to_bits(),
            0,
            0,
        ],
        table_words(&elem1),
        only_words(&only),
        first_words(&ones),
    ]
}

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u32; 4],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut Output,
) {
    *output = eval(input);
}
