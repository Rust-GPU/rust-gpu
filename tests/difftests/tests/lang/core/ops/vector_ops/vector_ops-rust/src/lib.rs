#![no_std]

use difftest::compat_round;
use spirv_std::glam::{UVec2, UVec3, UVec4, Vec2, Vec3, Vec4, Vec4Swizzles};
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

    // Read 4 floats from input
    let a = input[tid * 4];
    let b = input[tid * 4 + 1];
    let c = input[tid * 4 + 2];
    let d = input[tid * 4 + 3];

    let base_offset = tid * 50;

    if base_offset + 49 >= output.len() {
        return;
    }

    // Vec2 operations
    let v2a = Vec2::new(a, b);
    let v2b = Vec2::new(c, d);

    output[base_offset + 0] = compat_round!(v2a.dot(v2b));
    output[base_offset + 1] = compat_round!(v2a.length());
    output[base_offset + 2] = compat_round!(v2a.distance(v2b));

    let v2_add = v2a + v2b;
    output[base_offset + 3] = compat_round!(v2_add.x);
    output[base_offset + 4] = compat_round!(v2_add.y);

    let v2_mul = v2a * 2.0;
    output[base_offset + 5] = compat_round!(v2_mul.x);
    output[base_offset + 6] = compat_round!(v2_mul.y);

    // Vec3 operations
    let v3a = Vec3::new(a, b, c);
    let v3b = Vec3::new(b, c, d);

    output[base_offset + 7] = compat_round!(v3a.dot(v3b));
    output[base_offset + 8] = compat_round!(v3a.length());

    let v3_cross = v3a.cross(v3b);
    output[base_offset + 9] = compat_round!(v3_cross.x);
    output[base_offset + 10] = compat_round!(v3_cross.y);
    output[base_offset + 11] = compat_round!(v3_cross.z);

    let v3_norm = v3a.normalize();
    output[base_offset + 12] = compat_round!(v3_norm.x);
    output[base_offset + 13] = compat_round!(v3_norm.y);
    output[base_offset + 14] = compat_round!(v3_norm.z);

    // Vec4 operations
    let v4a = Vec4::new(a, b, c, d);
    let v4b = Vec4::new(d, c, b, a);

    output[base_offset + 15] = compat_round!(v4a.dot(v4b));
    output[base_offset + 16] = compat_round!(v4a.length());

    let v4_sub = v4a - v4b;
    output[base_offset + 17] = compat_round!(v4_sub.x);
    output[base_offset + 18] = compat_round!(v4_sub.y);
    output[base_offset + 19] = compat_round!(v4_sub.z);
    output[base_offset + 20] = compat_round!(v4_sub.w);

    // Swizzling
    output[base_offset + 21] = v4a.x;
    output[base_offset + 22] = v4a.y;
    output[base_offset + 23] = v4a.z;
    output[base_offset + 24] = v4a.w;

    let v4_swizzle = v4a.wzyx();
    output[base_offset + 25] = v4_swizzle.x;
    output[base_offset + 26] = v4_swizzle.y;
    output[base_offset + 27] = v4_swizzle.z;
    output[base_offset + 28] = v4_swizzle.w;

    // Component-wise operations
    let v4_min = v4a.min(v4b);
    output[base_offset + 29] = v4_min.x;
    output[base_offset + 30] = v4_min.y;
    output[base_offset + 31] = v4_min.z;

    // UVec operations
    let ua = a.abs() as u32;
    let ub = b.abs() as u32;
    let uc = c.abs() as u32;
    let ud = d.abs() as u32;

    let uv2 = UVec2::new(ua, ub);
    let uv3 = UVec3::new(ua, ub, uc);
    let uv4 = UVec4::new(ua, ub, uc, ud);

    output[base_offset + 32] = (uv2.x + uv2.y) as f32;
    output[base_offset + 33] = (uv3.x + uv3.y + uv3.z) as f32;
    output[base_offset + 34] = (uv4.x + uv4.y + uv4.z + uv4.w) as f32;

    // UVec min/max operations commented out due to Int8 requirement
    // See: https://github.com/Rust-GPU/rust-gpu/issues/314
    // let uv4_min = uv4.min(UVec4::new(ud, uc, ub, ua));
    // let uv4_max = uv4.max(UVec4::new(1, 2, 3, 4));
    output[base_offset + 35] = ua as f32; // Should be: uv4_min.x as f32
    output[base_offset + 36] = ub as f32; // Should be: uv4_min.y as f32
    output[base_offset + 37] = ua as f32; // Should be: uv4_max.x as f32
    output[base_offset + 38] = ub as f32; // Should be: uv4_max.y as f32
    output[base_offset + 39] = uc as f32; // Should be: uv4_max.z as f32

    // Vector constants
    output[base_offset + 40] = Vec2::ZERO.x;
    output[base_offset + 41] = Vec3::ONE.x;
    output[base_offset + 42] = Vec4::X.x;
    output[base_offset + 43] = Vec4::Y.y;

    // Converting between sizes
    let v2_from_v3 = v3a.truncate();
    output[base_offset + 44] = v2_from_v3.x;
    output[base_offset + 45] = v2_from_v3.y;

    let v3_from_v4 = v4a.truncate();
    output[base_offset + 46] = v3_from_v4.x;

    let v3_extended = v2a.extend(1.0);
    output[base_offset + 47] = v3_extended.z;

    let v4_extended = v3a.extend(2.0);
    output[base_offset + 48] = v4_extended.w;

    output[base_offset + 49] = UVec4::ONE.x as f32;
}
