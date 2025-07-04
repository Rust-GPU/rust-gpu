@group(0) @binding(0)
var<storage, read> input: array<f32>;

@group(0) @binding(1)
var<storage, read_write> output: array<f32>;

@compute @workgroup_size(32, 1, 1)
fn main_cs(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    
    if (tid >= 32u || tid >= arrayLength(&input)) {
        return;
    }
    
    let x = input[tid];
    let base_offset = tid * 21u;
    
    if (base_offset + 20u >= arrayLength(&output)) {
        return;
    }
    
    // Basic arithmetic
    output[base_offset + 0u] = x + 1.5;
    output[base_offset + 1u] = x - 0.5;
    output[base_offset + 2u] = x * 2.0;
    output[base_offset + 3u] = x / 2.0;
    output[base_offset + 4u] = x % 3.0;
    
    // Trigonometric functions (simplified for consistent results)
    output[base_offset + 5u] = sin(x);
    output[base_offset + 6u] = cos(x);
    output[base_offset + 7u] = clamp(tan(x), -10.0, 10.0);
    output[base_offset + 8u] = 0.0;
    output[base_offset + 9u] = 0.0;
    output[base_offset + 10u] = atan(x);
    
    // Exponential and logarithmic (simplified)
    output[base_offset + 11u] = min(exp(x), 1e6);
    output[base_offset + 12u] = select(-10.0, log(x), x > 0.0);
    output[base_offset + 13u] = sqrt(abs(x));
    output[base_offset + 14u] = pow(abs(x), 2.0);
    output[base_offset + 15u] = select(-10.0, log2(x), x > 0.0);
    output[base_offset + 16u] = min(exp2(x), 1e6);
    output[base_offset + 17u] = floor(x);
    output[base_offset + 18u] = ceil(x);
    output[base_offset + 19u] = round(x);
    
    // Special values and conversions
    let int_val = i32(x);
    output[base_offset + 20u] = f32(int_val);
}