@group(0) @binding(0)
var<storage, read_write> data: array<u32>;

@compute @workgroup_size(64)
fn main_cs(@builtin(global_invocation_id) id: vec3<u32>) {
    data[id.x] = id.x;
}
