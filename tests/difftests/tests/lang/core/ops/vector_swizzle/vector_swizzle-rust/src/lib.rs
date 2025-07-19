#![no_std]

use glam::{Vec3Swizzles, Vec4, Vec4Swizzles};
use spirv_std::spirv;

#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[[f32; 4]],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [[f32; 4]],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    let tid = global_id.x as usize;

    if tid < input.len() && tid < output.len() {
        let data = input[tid];
        let vec4 = Vec4::from_array(data);

        // Test various swizzle operations
        let result = match tid % 16 {
            // Vec4 swizzles
            0 => vec4.xyzw(), // Identity
            1 => vec4.wzyx(), // Reverse
            2 => vec4.xxxx(), // Broadcast
            3 => vec4.xzyw(), // Swap middle

            // Vec3 from Vec4
            4 => vec4.xyz().extend(0.0),
            5 => vec4.xyw().extend(0.0),
            6 => vec4.yzw().extend(0.0),

            // Vec2 from Vec4
            7 => {
                let v2 = vec4.xy();
                Vec4::new(v2.x, v2.y, 0.0, 0.0)
            }
            8 => {
                let v2 = vec4.zw();
                Vec4::new(v2.x, v2.y, 0.0, 0.0)
            }
            9 => {
                let v2 = vec4.xw();
                Vec4::new(v2.x, v2.y, 0.0, 0.0)
            }

            // Complex swizzles
            10 => {
                let v3 = vec4.zyx();
                v3.extend(vec4.w)
            }
            11 => {
                let v2 = vec4.yx();
                Vec4::new(v2.x, v2.y, vec4.z, vec4.w)
            }

            // Component access
            12 => Vec4::new(vec4.x, vec4.x, vec4.y, vec4.y),
            13 => Vec4::new(vec4.z, vec4.z, vec4.w, vec4.w),
            14 => Vec4::new(vec4.w, vec4.z, vec4.y, vec4.x),
            15 => {
                // Nested swizzles
                let v3 = vec4.xyz();
                let v2 = v3.xy();
                Vec4::new(v2.y, v2.x, v3.z, vec4.w)
            }
            _ => Vec4::ZERO,
        };

        output[tid] = result.to_array();
    }
}
