@group(0) @binding(0) var<storage, read> input: array<u32>;
@group(0) @binding(1) var<storage, read_write> output: array<u32>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    
    if (tid < arrayLength(&input) && tid < arrayLength(&output)) {
        let value = input[tid];
        
        // Complex control flow with nested loops, early returns, and switch expressions
        let result = process_value(value, tid);
        
        output[tid] = result;
    }
}

fn process_value(value: u32, tid: u32) -> u32 {
    // Early return for special cases
    if (value == 0u) {
        return 0u;
    }
    
    if (value > 1000u) {
        return select(0u, value - 1000u, value >= 1000u); // saturating_sub equivalent
    }
    
    // Complex switch expression
    var base: u32;
    switch (value % 10u) {
        case 0u: {
            base = 10u;
        }
        case 1u, 2u: {
            base = value * 2u;
        }
        case 3u, 4u, 5u: {
            // Nested computation
            var sum = 0u;
            for (var i = 0u; i < 3u; i = i + 1u) {
                sum = sum + value + i;
            }
            base = sum;
        }
        case 6u: {
            // Early return from switch case
            if (tid % 2u == 0u) {
                return value * 3u;
            }
            base = value * 4u;
        }
        case 7u, 8u: {
            // Nested loop with break
            var result = 0u;
            for (var i = 0u; i < 5u; i = i + 1u) {
                for (var j = 0u; j < 3u; j = j + 1u) {
                    result = result + i * j;
                    if (result > value) {
                        i = 5u; // Force outer loop to exit
                        break;
                    }
                }
                if (i >= 5u) {
                    break;
                }
            }
            base = result;
        }
        case 9u: {
            // Complex nested condition
            if (tid < 10u) {
                if (value < 50u) {
                    base = value + tid;
                } else {
                    base = value - tid;
                }
            } else {
                // Loop with continue
                var sum = 0u;
                for (var i = 0u; i < value; i = i + 1u) {
                    if (i % 3u == 0u) {
                        continue;
                    }
                    sum = sum + i;
                    if (sum > 100u) {
                        break;
                    }
                }
                base = sum;
            }
        }
        default: {
            base = value;
        }
    }
    
    // Final transformation with nested conditions
    if (base > 50u) {
        if (tid % 3u == 0u) {
            return base / 2u;
        } else if (tid % 3u == 1u) {
            return base * 2u;
        } else {
            return base;
        }
    } else {
        // Another loop with multiple exit conditions
        var result = base;
        var counter = 0u;
        loop {
            result = (result * 3u + 1u) / 2u;
            counter = counter + 1u;
            
            if (result == 1u || counter >= 10u || result > 1000u) {
                break;
            }
        }
        return result + counter;
    }
}