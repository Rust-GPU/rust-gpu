#![no_std]

use difftest::round6;
use spirv_std::glam::{Mat2, Mat3, Mat4, UVec3, Vec2, Vec3, Vec4};
#[allow(unused_imports)]
use spirv_std::num_traits::Float;
use spirv_std::spirv;

#[spirv(compute(threads(32)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[f32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [f32],
    #[spirv(global_invocation_id)] global_id: UVec3,
) {
    let tid = global_id.x as usize;

    if tid >= 32 || tid * 4 + 3 >= input.len() {
        return;
    }

    // Read input values
    let a = input[tid * 4];
    let b = input[tid * 4 + 1];
    let c = input[tid * 4 + 2];
    let d = input[tid * 4 + 3];

    let base_offset = tid * 50;

    if base_offset + 49 >= output.len() {
        return;
    }

    // Mat2 operations
    let m2a = Mat2::from_cols(Vec2::new(a, b), Vec2::new(c, d));
    let m2b = Mat2::from_cols(Vec2::new(d, c), Vec2::new(b, a));

    // Mat2 multiplication
    let m2_mul = m2a * m2b;
    output[base_offset + 0] = round6!(m2_mul.col(0).x);
    output[base_offset + 1] = round6!(m2_mul.col(0).y);
    output[base_offset + 2] = round6!(m2_mul.col(1).x);
    output[base_offset + 3] = round6!(m2_mul.col(1).y);

    // Mat2 transpose
    let m2_transpose = m2a.transpose();
    output[base_offset + 4] = m2_transpose.col(0).x;
    output[base_offset + 5] = m2_transpose.col(0).y;
    output[base_offset + 6] = m2_transpose.col(1).x;
    output[base_offset + 7] = m2_transpose.col(1).y;

    // Mat2 determinant (with rounding for consistency)
    output[base_offset + 8] = round6!(m2a.determinant());

    // Mat2 * Vec2
    let v2 = Vec2::new(1.0, 2.0);
    let m2_v2 = m2a * v2;
    output[base_offset + 9] = round6!(m2_v2.x);
    output[base_offset + 10] = round6!(m2_v2.y);

    // Mat3 operations
    let m3a = Mat3::from_cols(Vec3::new(a, b, c), Vec3::new(b, c, d), Vec3::new(c, d, a));
    let m3b = Mat3::from_cols(Vec3::new(d, c, b), Vec3::new(c, b, a), Vec3::new(b, a, d));

    // Mat3 multiplication
    let m3_mul = m3a * m3b;
    output[base_offset + 11] = round6!(m3_mul.col(0).x);
    output[base_offset + 12] = round6!(m3_mul.col(0).y);
    output[base_offset + 13] = round6!(m3_mul.col(0).z);
    output[base_offset + 14] = round6!(m3_mul.col(1).x);
    output[base_offset + 15] = round6!(m3_mul.col(1).y);
    output[base_offset + 16] = round6!(m3_mul.col(1).z);
    output[base_offset + 17] = round6!(m3_mul.col(2).x);
    output[base_offset + 18] = round6!(m3_mul.col(2).y);
    output[base_offset + 19] = round6!(m3_mul.col(2).z);

    // Mat3 transpose - store just diagonal elements
    let m3_transpose = m3a.transpose();
    output[base_offset + 20] = m3_transpose.col(0).x;
    output[base_offset + 21] = m3_transpose.col(1).y;
    output[base_offset + 22] = m3_transpose.col(2).z;

    // Mat3 determinant (with rounding for consistency)
    output[base_offset + 23] = round6!(m3a.determinant());

    // Mat3 * Vec3 (with rounding for consistency)
    let v3 = Vec3::new(1.0, 2.0, 3.0);
    let m3_v3 = m3a * v3;
    output[base_offset + 24] = round6!(m3_v3.x);
    output[base_offset + 25] = round6!(m3_v3.y);
    output[base_offset + 26] = round6!(m3_v3.z);

    // Mat4 operations
    let m4a = Mat4::from_cols(
        Vec4::new(a, b, c, d),
        Vec4::new(b, c, d, a),
        Vec4::new(c, d, a, b),
        Vec4::new(d, a, b, c),
    );
    let m4b = Mat4::from_cols(
        Vec4::new(d, c, b, a),
        Vec4::new(c, b, a, d),
        Vec4::new(b, a, d, c),
        Vec4::new(a, d, c, b),
    );

    // Mat4 multiplication (just store diagonal for brevity)
    let m4_mul = m4a * m4b;
    output[base_offset + 27] = round6!(m4_mul.col(0).x);
    output[base_offset + 28] = round6!(m4_mul.col(1).y);
    output[base_offset + 29] = round6!(m4_mul.col(2).z);
    output[base_offset + 30] = round6!(m4_mul.col(3).w);

    // Mat4 transpose (just store diagonal)
    let m4_transpose = m4a.transpose();
    output[base_offset + 31] = m4_transpose.col(0).x;
    output[base_offset + 32] = m4_transpose.col(1).y;
    output[base_offset + 33] = m4_transpose.col(2).z;
    output[base_offset + 34] = m4_transpose.col(3).w;

    // Mat4 determinant (with rounding for consistency)
    output[base_offset + 35] = round6!(m4a.determinant());

    // Mat4 * Vec4 (with rounding for consistency)
    let v4 = Vec4::new(1.0, 2.0, 3.0, 4.0);
    let m4_v4 = m4a * v4;
    output[base_offset + 36] = round6!(m4_v4.x);
    output[base_offset + 37] = round6!(m4_v4.y);
    output[base_offset + 38] = round6!(m4_v4.z);
    output[base_offset + 39] = round6!(m4_v4.w);

    // Identity matrices
    output[base_offset + 40] = Mat2::IDENTITY.col(0).x;
    output[base_offset + 41] = Mat3::IDENTITY.col(1).y;
    output[base_offset + 42] = Mat4::IDENTITY.col(2).z;

    // Matrix inverse
    if m2a.determinant().abs() > 0.0001 {
        let m2_inv = m2a.inverse();
        output[base_offset + 43] = round6!(m2_inv.col(0).x);
        output[base_offset + 44] = round6!(m2_inv.col(1).y);
    } else {
        output[base_offset + 43] = 0.0;
        output[base_offset + 44] = 0.0;
    }

    // Matrix addition
    let m2_add = m2a + m2b;
    output[base_offset + 45] = m2_add.col(0).x;
    output[base_offset + 46] = m2_add.col(0).y;

    // Matrix scalar multiplication
    let m2_scale = m2a * 2.0;
    output[base_offset + 47] = m2_scale.col(0).x;
    output[base_offset + 48] = m2_scale.col(0).y;

    output[base_offset + 49] = 1.0; // Padding
}
