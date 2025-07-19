struct PushConstants {
    multiplier: f32,
    offset: f32,
    flags: u32,
    count: u32,
}

var<push_constant> push_constants: PushConstants;

@group(0) @binding(0)
var<storage, read> input: array<f32>;

@group(0) @binding(1)
var<storage, read_write> output: array<f32>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    let count = push_constants.count;
    
    if (tid < arrayLength(&input) && tid < arrayLength(&output) && tid < count) {
        let value = input[tid];
        var result: f32;
        
        // Apply different operations based on flags
        switch push_constants.flags {
            case 0u: {
                // Linear transformation
                result = value * push_constants.multiplier + push_constants.offset;
            }
            case 1u: {
                // Quadratic transformation
                result = value * value * push_constants.multiplier + push_constants.offset;
            }
            case 2u: {
                // Sine wave modulation
                result = sin(value * push_constants.multiplier) + push_constants.offset;
            }
            case 3u: {
                // Exponential transformation
                result = exp(value * push_constants.multiplier) + push_constants.offset;
            }
            case 4u: {
                // Logarithmic transformation (with protection against negative values)
                if (value > 0.0) {
                    result = log(value * push_constants.multiplier) + push_constants.offset;
                } else {
                    result = push_constants.offset;
                }
            }
            case 5u: {
                // Reciprocal transformation (with protection against division by zero)
                if (abs(value) > 0.001) {
                    result = push_constants.multiplier / value + push_constants.offset;
                } else {
                    result = push_constants.offset;
                }
            }
            case 6u: {
                // Power transformation
                result = pow(value, push_constants.multiplier) + push_constants.offset;
            }
            case 7u: {
                // Modulo operation (treating multiplier as divisor)
                if (push_constants.multiplier > 0.0) {
                    result = (value % push_constants.multiplier) + push_constants.offset;
                } else {
                    result = push_constants.offset;
                }
            }
            default: {
                // Default: just add offset
                result = value + push_constants.offset;
            }
        }
        
        output[tid] = result;
    }
}