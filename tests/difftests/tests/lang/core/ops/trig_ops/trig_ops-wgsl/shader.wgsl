@group(0) @binding(0) var<storage, read> input: array<f32>;
@group(0) @binding(1) var<storage, read_write> output: array<f32>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    
    if (tid < arrayLength(&input) && tid < arrayLength(&output)) {
        let x = input[tid];
        
        // Test various trigonometric functions
        var result: f32;
        switch (tid % 14u) {
            case 0u: { result = sin(x); }
            case 1u: { result = cos(x); }
            case 2u: { result = tan(x); }
            case 3u: { result = asin(clamp(x, -1.0, 1.0)); } // Clamp to avoid NaN
            case 4u: { result = acos(clamp(x, -1.0, 1.0)); } // Clamp to avoid NaN
            case 5u: { result = atan(x); }
            case 6u: { result = sinh(x); }
            case 7u: { result = cosh(x); }
            case 8u: { result = tanh(x); }
            case 9u: { 
                // atan2 - use two consecutive values
                var y: f32 = 1.0;
                if (tid + 1u < arrayLength(&input)) {
                    y = input[tid + 1u];
                }
                result = atan2(x, y);
            }
            case 10u: { result = sqrt(x * x + 1.0); } // hypot equivalent
            case 11u: { result = radians(x); }
            case 12u: { result = degrees(x); }
            case 13u: { 
                // sincos - return sin for even indices, cos for odd
                if (tid % 2u == 0u) {
                    result = sin(x);
                } else {
                    result = cos(x);
                }
            }
            default: { result = 0.0; }
        }
        
        output[tid] = result;
    }
}