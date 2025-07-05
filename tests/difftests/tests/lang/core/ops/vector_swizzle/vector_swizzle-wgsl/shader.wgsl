@group(0) @binding(0) var<storage, read> input: array<vec4<f32>>;
@group(0) @binding(1) var<storage, read_write> output: array<vec4<f32>>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    
    if (tid < arrayLength(&input) && tid < arrayLength(&output)) {
        let vec4_val = input[tid];
        
        // Test various swizzle operations
        var result: vec4<f32>;
        switch (tid % 16u) {
            // Vec4 swizzles
            case 0u: { result = vec4_val.xyzw; }      // Identity
            case 1u: { result = vec4_val.wzyx; }      // Reverse
            case 2u: { result = vec4_val.xxxx; }      // Broadcast
            case 3u: { result = vec4_val.xzyw; }      // Swap middle
            
            // Vec3 from Vec4
            case 4u: { result = vec4<f32>(vec4_val.xyz, 0.0); }
            case 5u: { result = vec4<f32>(vec4_val.xyw, 0.0); }
            case 6u: { result = vec4<f32>(vec4_val.yzw, 0.0); }
            
            // Vec2 from Vec4
            case 7u: { 
                let v2 = vec4_val.xy;
                result = vec4<f32>(v2.x, v2.y, 0.0, 0.0);
            }
            case 8u: { 
                let v2 = vec4_val.zw;
                result = vec4<f32>(v2.x, v2.y, 0.0, 0.0);
            }
            case 9u: { 
                let v2 = vec4_val.xw;
                result = vec4<f32>(v2.x, v2.y, 0.0, 0.0);
            }
            
            // Complex swizzles
            case 10u: { 
                let v3 = vec4_val.zyx;
                result = vec4<f32>(v3, vec4_val.w);
            }
            case 11u: { 
                let v2 = vec4_val.yx;
                result = vec4<f32>(v2.x, v2.y, vec4_val.z, vec4_val.w);
            }
            
            // Component access
            case 12u: { result = vec4<f32>(vec4_val.x, vec4_val.x, vec4_val.y, vec4_val.y); }
            case 13u: { result = vec4<f32>(vec4_val.z, vec4_val.z, vec4_val.w, vec4_val.w); }
            case 14u: { result = vec4<f32>(vec4_val.w, vec4_val.z, vec4_val.y, vec4_val.x); }
            case 15u: { 
                // Nested swizzles
                let v3 = vec4_val.xyz;
                let v2 = v3.xy;
                result = vec4<f32>(v2.y, v2.x, v3.z, vec4_val.w);
            }
            default: { result = vec4<f32>(0.0); }
        }
        
        output[tid] = result;
    }
}