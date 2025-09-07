#![no_std]

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main_cs(#[spirv(storage_buffer, descriptor_set = 0, binding = 0)] out: &mut [u32]) {
    let x = 42i32;
    let y = x as u32;

    let a = 3.14f32;
    let b = a.to_bits();

    let v = [1u32, 2, 3, 4];
    let w = v;

    let result = 10 + 20;

    let data = 100u32;
    let ref_data = &data;

    let pair = (5u32, 6u32);
    let pair_sum = pair.0 + pair.1;

    let values = [
        y, b, w[0], w[1], w[2], w[3], result, *ref_data, pair_sum, 0, 0, 0,
    ];
    out[0] = values[0];
    out[1] = values[1];
    out[2] = values[2];
    out[3] = values[3];
    out[4] = values[4];
    out[5] = values[5];
    out[6] = values[6];
    out[7] = values[7];
    out[8] = values[8];
    out[9] = values[9];
    out[10] = values[10];
    out[11] = values[11];
}
