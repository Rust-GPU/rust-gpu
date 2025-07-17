@group(0) @binding(0)
var<storage, read> input: array<u32>;

@group(0) @binding(1)
var<storage, read_write> output: array<u32>;

fn complex_function(x: u32) -> u32 {
    if (x == 0u) {
        return 999u;
    }
    
    for (var i = 0u; i < x; i = i + 1u) {
        if (i > 5u) {
            return i * 100u;
        }
    }
    
    var result = x;
    while (result > 10u) {
        result = result / 2u;
        if (result == 15u) {
            return 777u;
        }
    }
    
    return result;
}

@compute @workgroup_size(64, 1, 1)
fn main_cs(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let tid = global_id.x;
    let output_len = arrayLength(&output);
    
    if (tid >= output_len) {
        return; // Early return test
    }
    
    let val = select(0u, input[tid], tid < arrayLength(&input));
    
    // Test 1: Simple if/else
    var result1: u32;
    if (val % 2u == 0u) {
        result1 = val * 2u;
    } else {
        result1 = val * 3u;
    }
    
    // Test 2: Nested if/else
    var result2: u32;
    if (val < 10u) {
        if (val < 5u) {
            result2 = val + 100u;
        } else {
            result2 = val + 200u;
        }
    } else if (val < 20u) {
        result2 = val + 300u;
    } else {
        result2 = val + 400u;
    }
    
    // Test 3: Loop with break
    var sum = 0u;
    var i = 0u;
    loop {
        if (i >= val || i >= 10u) {
            break;
        }
        sum = sum + i;
        i = i + 1u;
    }
    
    // Test 4: While loop
    var product = 1u;
    var j = 1u;
    while (j <= 5u && j <= val) {
        product = product * j;
        j = j + 1u;
    }
    
    // Test 5: For loop with continue
    var count = 0u;
    for (var k = 0u; k < 20u; k = k + 1u) {
        if (k % 3u == 0u) {
            continue;
        }
        if (k > val) {
            break;
        }
        count = count + 1u;
    }
    
    // Test 6: Switch expression (WGSL's equivalent of match)
    var match_result: u32;
    switch (val % 4u) {
        case 0u: {
            match_result = 1000u;
        }
        case 1u: {
            match_result = 2000u;
        }
        case 2u: {
            match_result = 3000u;
        }
        default: {
            match_result = 4000u;
        }
    }
    
    // Test 7: Complex control flow with early returns
    let complex_result = complex_function(val);
    
    // Combine all results (using addition with overflow wrapping)
    output[tid] = (result1 + result2 + sum + product + count + match_result + complex_result) & 0xFFFFFFFFu;
}