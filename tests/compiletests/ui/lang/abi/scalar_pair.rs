// build-pass
// compile-flags: -C target-feature=+Int64

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main_future_proof(
    #[spirv(flat)] input: (u64, u32),
    out: &mut (u64, u32),
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] buffer_in: &(u64, u32),
    #[spirv(storage_buffer, descriptor_set = 1, binding = 0)] buffer_out: &mut (u64, u32),
) {
    *out = trans0(trans_ref(buffer_in));
    *buffer_out = trans1(input);
}

pub fn trans0(arg: (u64, u32)) -> (u64, u32) {
    (arg.0 + 1, arg.1 - 1)
}

pub fn trans1((a, b): (u64, u32)) -> (u64, u32) {
    (a * 2, b * 3)
}

pub fn trans_ref((a, b): &(u64, u32)) -> (u64, u32) {
    (a - 1, b - 1)
}
