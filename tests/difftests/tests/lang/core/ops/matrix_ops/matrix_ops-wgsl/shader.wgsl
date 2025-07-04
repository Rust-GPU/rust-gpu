@group(0) @binding(0)
var<storage, read> input: array<f32>;

@group(0) @binding(1)
var<storage, read_write> output: array<f32>;

@compute @workgroup_size(32, 1, 1)
fn main_cs(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    
    if (tid >= 32u || tid * 4u + 3u >= arrayLength(&input)) {
        return;
    }
    
    // Read input values
    let a = input[tid * 4u];
    let b = input[tid * 4u + 1u];
    let c = input[tid * 4u + 2u];
    let d = input[tid * 4u + 3u];
    
    let base_offset = tid * 50u;
    
    if (base_offset + 49u >= arrayLength(&output)) {
        return;
    }
    
    // Mat2 operations
    let m2a = mat2x2<f32>(
        vec2<f32>(a, b),
        vec2<f32>(c, d)
    );
    let m2b = mat2x2<f32>(
        vec2<f32>(d, c),
        vec2<f32>(b, a)
    );
    
    // Mat2 multiplication
    let m2_mul = m2a * m2b;
    output[base_offset + 0u] = m2_mul[0].x;
    output[base_offset + 1u] = m2_mul[0].y;
    output[base_offset + 2u] = m2_mul[1].x;
    output[base_offset + 3u] = m2_mul[1].y;
    
    // Mat2 transpose
    let m2_transpose = transpose(m2a);
    output[base_offset + 4u] = m2_transpose[0].x;
    output[base_offset + 5u] = m2_transpose[0].y;
    output[base_offset + 6u] = m2_transpose[1].x;
    output[base_offset + 7u] = m2_transpose[1].y;
    
    // Mat2 determinant (with rounding for consistency)
    output[base_offset + 8u] = round(determinant(m2a) * 1000.0) / 1000.0;
    
    // Mat2 * Vec2
    let v2 = vec2<f32>(1.0, 2.0);
    let m2_v2 = m2a * v2;
    output[base_offset + 9u] = m2_v2.x;
    output[base_offset + 10u] = m2_v2.y;
    
    // Mat3 operations
    let m3a = mat3x3<f32>(
        vec3<f32>(a, b, c),
        vec3<f32>(b, c, d),
        vec3<f32>(c, d, a)
    );
    let m3b = mat3x3<f32>(
        vec3<f32>(d, c, b),
        vec3<f32>(c, b, a),
        vec3<f32>(b, a, d)
    );
    
    // Mat3 multiplication
    let m3_mul = m3a * m3b;
    output[base_offset + 11u] = m3_mul[0].x;
    output[base_offset + 12u] = m3_mul[0].y;
    output[base_offset + 13u] = m3_mul[0].z;
    output[base_offset + 14u] = m3_mul[1].x;
    output[base_offset + 15u] = m3_mul[1].y;
    output[base_offset + 16u] = m3_mul[1].z;
    output[base_offset + 17u] = m3_mul[2].x;
    output[base_offset + 18u] = m3_mul[2].y;
    output[base_offset + 19u] = m3_mul[2].z;
    
    // Mat3 transpose
    let m3_transpose = transpose(m3a);
    output[base_offset + 20u] = m3_transpose[0].x;
    output[base_offset + 21u] = m3_transpose[1].y;
    output[base_offset + 22u] = m3_transpose[2].z;
    
    // Mat3 determinant (with rounding for consistency)
    output[base_offset + 23u] = round(determinant(m3a) * 1000.0) / 1000.0;
    
    // Mat3 * Vec3 (with rounding for consistency)
    let v3 = vec3<f32>(1.0, 2.0, 3.0);
    let m3_v3 = m3a * v3;
    output[base_offset + 24u] = round(m3_v3.x * 10000.0) / 10000.0;
    output[base_offset + 25u] = round(m3_v3.y * 10000.0) / 10000.0;
    output[base_offset + 26u] = round(m3_v3.z * 10000.0) / 10000.0;
    
    // Mat4 operations
    let m4a = mat4x4<f32>(
        vec4<f32>(a, b, c, d),
        vec4<f32>(b, c, d, a),
        vec4<f32>(c, d, a, b),
        vec4<f32>(d, a, b, c)
    );
    let m4b = mat4x4<f32>(
        vec4<f32>(d, c, b, a),
        vec4<f32>(c, b, a, d),
        vec4<f32>(b, a, d, c),
        vec4<f32>(a, d, c, b)
    );
    
    // Mat4 multiplication (just store diagonal for brevity)
    let m4_mul = m4a * m4b;
    output[base_offset + 27u] = m4_mul[0].x;
    output[base_offset + 28u] = m4_mul[1].y;
    output[base_offset + 29u] = m4_mul[2].z;
    output[base_offset + 30u] = m4_mul[3].w;
    
    // Mat4 transpose (just store diagonal)
    let m4_transpose = transpose(m4a);
    output[base_offset + 31u] = m4_transpose[0].x;
    output[base_offset + 32u] = m4_transpose[1].y;
    output[base_offset + 33u] = m4_transpose[2].z;
    output[base_offset + 34u] = m4_transpose[3].w;
    
    // Mat4 determinant (with rounding for consistency)
    output[base_offset + 35u] = round(determinant(m4a) * 1000.0) / 1000.0;
    
    // Mat4 * Vec4 (with rounding for consistency)
    let v4 = vec4<f32>(1.0, 2.0, 3.0, 4.0);
    let m4_v4 = m4a * v4;
    output[base_offset + 36u] = round(m4_v4.x * 10000.0) / 10000.0;
    output[base_offset + 37u] = round(m4_v4.y * 10000.0) / 10000.0;
    output[base_offset + 38u] = round(m4_v4.z * 10000.0) / 10000.0;
    output[base_offset + 39u] = round(m4_v4.w * 10000.0) / 10000.0;
    
    // Identity matrices
    output[base_offset + 40u] = mat2x2<f32>(vec2<f32>(1.0, 0.0), vec2<f32>(0.0, 1.0))[0].x;
    output[base_offset + 41u] = mat3x3<f32>(vec3<f32>(1.0, 0.0, 0.0), vec3<f32>(0.0, 1.0, 0.0), vec3<f32>(0.0, 0.0, 1.0))[1].y;
    output[base_offset + 42u] = mat4x4<f32>(vec4<f32>(1.0, 0.0, 0.0, 0.0), vec4<f32>(0.0, 1.0, 0.0, 0.0), vec4<f32>(0.0, 0.0, 1.0, 0.0), vec4<f32>(0.0, 0.0, 0.0, 1.0))[2].z;
    
    // Matrix inverse (WGSL doesn't have try_inverse, so we check determinant)
    let det = determinant(m2a);
    if (abs(det) > 0.0001) {
        // Manual 2x2 inverse calculation
        let inv_det = 1.0 / det;
        let m2_inv = mat2x2<f32>(
            vec2<f32>(m2a[1].y * inv_det, -m2a[0].y * inv_det),
            vec2<f32>(-m2a[1].x * inv_det, m2a[0].x * inv_det)
        );
        output[base_offset + 43u] = m2_inv[0].x;
        output[base_offset + 44u] = m2_inv[1].y;
    } else {
        output[base_offset + 43u] = 0.0;
        output[base_offset + 44u] = 0.0;
    }
    
    // Matrix addition
    let m2_add = mat2x2<f32>(
        m2a[0] + m2b[0],
        m2a[1] + m2b[1]
    );
    output[base_offset + 45u] = m2_add[0].x;
    output[base_offset + 46u] = m2_add[0].y;
    
    // Matrix scalar multiplication
    let m2_scale = mat2x2<f32>(
        m2a[0] * 2.0,
        m2a[1] * 2.0
    );
    output[base_offset + 47u] = m2_scale[0].x;
    output[base_offset + 48u] = m2_scale[0].y;
    
    output[base_offset + 49u] = 1.0; // Padding
}