@group(0) @binding(0)
var<storage, read> input: array<f32>;

@group(0) @binding(1)
var<storage, read_write> output: array<f32>;

// Helper function to round to 5 decimal places for cross-platform compatibility
fn compat_round(v: f32) -> f32 {
    return round(v * 100000.0) / 100000.0;
}

@compute @workgroup_size(32, 1, 1)
fn main_cs(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    
    if (tid >= 32u || tid * 4u + 3u >= arrayLength(&input)) {
        return;
    }
    
    // Read 4 floats from input
    let a = input[tid * 4u];
    let b = input[tid * 4u + 1u];
    let c = input[tid * 4u + 2u];
    let d = input[tid * 4u + 3u];
    
    let base_offset = tid * 50u;
    
    if (base_offset + 49u >= arrayLength(&output)) {
        return;
    }
    
    // Vec2 operations
    let v2a = vec2<f32>(a, b);
    let v2b = vec2<f32>(c, d);
    
    output[base_offset + 0u] = compat_round(dot(v2a, v2b));
    output[base_offset + 1u] = compat_round(length(v2a));
    output[base_offset + 2u] = compat_round(distance(v2a, v2b));
    
    let v2_add = v2a + v2b;
    output[base_offset + 3u] = compat_round(v2_add.x);
    output[base_offset + 4u] = compat_round(v2_add.y);
    
    let v2_mul = v2a * 2.0;
    output[base_offset + 5u] = compat_round(v2_mul.x);
    output[base_offset + 6u] = compat_round(v2_mul.y);
    
    // Vec3 operations
    let v3a = vec3<f32>(a, b, c);
    let v3b = vec3<f32>(b, c, d);
    
    output[base_offset + 7u] = compat_round(dot(v3a, v3b));
    output[base_offset + 8u] = compat_round(length(v3a));
    
    let v3_cross = cross(v3a, v3b);
    output[base_offset + 9u] = compat_round(v3_cross.x);
    output[base_offset + 10u] = compat_round(v3_cross.y);
    output[base_offset + 11u] = compat_round(v3_cross.z);
    
    let v3_norm = normalize(v3a);
    output[base_offset + 12u] = compat_round(v3_norm.x);
    output[base_offset + 13u] = compat_round(v3_norm.y);
    output[base_offset + 14u] = compat_round(v3_norm.z);
    
    // Vec4 operations
    let v4a = vec4<f32>(a, b, c, d);
    let v4b = vec4<f32>(d, c, b, a);
    
    output[base_offset + 15u] = compat_round(dot(v4a, v4b));
    output[base_offset + 16u] = compat_round(length(v4a));
    
    let v4_sub = v4a - v4b;
    output[base_offset + 17u] = compat_round(v4_sub.x);
    output[base_offset + 18u] = compat_round(v4_sub.y);
    output[base_offset + 19u] = compat_round(v4_sub.z);
    output[base_offset + 20u] = compat_round(v4_sub.w);
    
    // Swizzling
    output[base_offset + 21u] = v4a.x;
    output[base_offset + 22u] = v4a.y;
    output[base_offset + 23u] = v4a.z;
    output[base_offset + 24u] = v4a.w;
    
    let v4_swizzle = v4a.wzyx;
    output[base_offset + 25u] = v4_swizzle.x;
    output[base_offset + 26u] = v4_swizzle.y;
    output[base_offset + 27u] = v4_swizzle.z;
    output[base_offset + 28u] = v4_swizzle.w;
    
    // Component-wise operations
    let v4_min = min(v4a, v4b);
    output[base_offset + 29u] = v4_min.x;
    output[base_offset + 30u] = v4_min.y;
    output[base_offset + 31u] = v4_min.z;
    
    // UVec operations
    let ua = u32(abs(a));
    let ub = u32(abs(b));
    let uc = u32(abs(c));
    let ud = u32(abs(d));
    
    let uv2 = vec2<u32>(ua, ub);
    let uv3 = vec3<u32>(ua, ub, uc);
    let uv4 = vec4<u32>(ua, ub, uc, ud);
    
    output[base_offset + 32u] = f32(uv2.x + uv2.y);
    output[base_offset + 33u] = f32(uv3.x + uv3.y + uv3.z);
    output[base_offset + 34u] = f32(uv4.x + uv4.y + uv4.z + uv4.w);
    
    // UVec min/max operations commented out to match Rust side
    // See: https://github.com/Rust-GPU/rust-gpu/issues/314
    // let uv4_min = min(uv4, vec4<u32>(ud, uc, ub, ua));
    // let uv4_max = max(uv4, vec4<u32>(1u, 2u, 3u, 4u));
    output[base_offset + 35u] = f32(ua); // Should be: f32(uv4_min.x)
    output[base_offset + 36u] = f32(ub); // Should be: f32(uv4_min.y)
    output[base_offset + 37u] = f32(ua); // Should be: f32(uv4_max.x)
    output[base_offset + 38u] = f32(ub); // Should be: f32(uv4_max.y)
    output[base_offset + 39u] = f32(uc); // Should be: f32(uv4_max.z)
    
    // Vector constants  
    output[base_offset + 40u] = vec2<f32>(0.0, 0.0).x;
    output[base_offset + 41u] = vec3<f32>(1.0, 1.0, 1.0).x;
    output[base_offset + 42u] = vec4<f32>(1.0, 0.0, 0.0, 0.0).x;
    output[base_offset + 43u] = vec4<f32>(0.0, 1.0, 0.0, 0.0).y;
    
    // Converting between sizes
    let v2_from_v3 = vec2<f32>(v3a.xy);
    output[base_offset + 44u] = v2_from_v3.x;
    output[base_offset + 45u] = v2_from_v3.y;
    
    let v3_from_v4 = vec3<f32>(v4a.xyz);
    output[base_offset + 46u] = v3_from_v4.x;
    
    let v3_extended = vec3<f32>(v2a, 1.0);
    output[base_offset + 47u] = v3_extended.z;
    
    let v4_extended = vec4<f32>(v3a, 2.0);
    output[base_offset + 48u] = v4_extended.w;
    
    output[base_offset + 49u] = f32(vec4<u32>(1u, 1u, 1u, 1u).x);
}
