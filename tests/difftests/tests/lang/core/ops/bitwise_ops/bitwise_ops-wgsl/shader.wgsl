@group(0) @binding(0) var<storage, read> input_a: array<u32>;
@group(0) @binding(1) var<storage, read> input_b: array<u32>;
@group(0) @binding(2) var<storage, read_write> output: array<u32>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    
    if (tid < arrayLength(&input_a) && tid < arrayLength(&input_b) && tid < arrayLength(&output)) {
        let a = input_a[tid];
        let b = input_b[tid];
        
        // Test various bitwise operations
        var result: u32;
        switch (tid % 12u) {
            case 0u: { result = a & b; }           // AND
            case 1u: { result = a | b; }           // OR
            case 2u: { result = a ^ b; }           // XOR
            case 3u: { result = ~a; }              // NOT
            case 4u: { result = a << (b % 32u); }  // Left shift (avoid UB with modulo)
            case 5u: { result = a >> (b % 32u); }  // Right shift (avoid UB with modulo)
            case 6u: { 
                // Rotate left
                let shift = b % 32u;
                result = (a << shift) | (a >> (32u - shift));
            }
            case 7u: { 
                // Rotate right
                let shift = b % 32u;
                result = (a >> shift) | (a << (32u - shift));
            }
            case 8u: { result = countOneBits(a); }      // Population count
            case 9u: { result = countLeadingZeros(a); }  // Leading zeros
            case 10u: { result = countTrailingZeros(a); } // Trailing zeros
            case 11u: { result = reverseBits(a); }       // Bit reversal
            default: { result = 0u; }
        }
        
        output[tid] = result;
    }
}