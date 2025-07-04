@group(0) @binding(0)
var<storage, read> input: array<f32>;

@group(0) @binding(1)
var<storage, read_write> output: array<f32>;

// Helper function to round to 6 decimal places for cross-platform consistency
fn round6(v: f32) -> f32 {
    return round(v * 1000000.0) / 1000000.0;
}

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
    output[base_offset + 0u] = round6(x + 1.5);
    output[base_offset + 1u] = round6(x - 0.5);
    output[base_offset + 2u] = round6(x * 2.0);
    output[base_offset + 3u] = round6(x / 2.0);
    output[base_offset + 4u] = round6(x % 3.0);
    
    // Trigonometric functions (simplified for consistent results)
    output[base_offset + 5u] = round6(sin(x));
    output[base_offset + 6u] = round6(cos(x));
    output[base_offset + 7u] = round6(clamp(tan(x), -10.0, 10.0));
    output[base_offset + 8u] = 0.0;
    output[base_offset + 9u] = 0.0;
    output[base_offset + 10u] = round6(atan(x));
    
    // Exponential and logarithmic (simplified)
    output[base_offset + 11u] = round6(min(exp(x), 1e6));
    output[base_offset + 12u] = round6(select(-10.0, log(x), x > 0.0));
    output[base_offset + 13u] = round6(sqrt(abs(x)));
    output[base_offset + 14u] = round6(abs(x) * abs(x)); // Use multiplication instead of pow
    output[base_offset + 15u] = round6(select(-10.0, log2(x), x > 0.0));
    output[base_offset + 16u] = round6(min(exp2(x), 1e6));
    output[base_offset + 17u] = floor(x); // floor/ceil/round are exact
    output[base_offset + 18u] = ceil(x);
    output[base_offset + 19u] = round(x);
    
    // Special values and conversions
    let int_val = i32(x);
    output[base_offset + 20u] = f32(int_val);
}