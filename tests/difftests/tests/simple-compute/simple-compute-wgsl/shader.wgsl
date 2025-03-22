@group(0) @binding(0)
var<storage, read_write> output: array<u32>;

@compute @workgroup_size(1)
fn main_cs() {
    output[0] = 42u;
}
