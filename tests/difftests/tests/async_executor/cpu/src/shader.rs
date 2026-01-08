use crate::common::eval;
use spirv_std::spirv;

/// ASSUMPTION
/// subgroup size is at least this
const WG_SIZE: u32 = 32;

#[spirv(compute(threads(32)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] input: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(local_invocation_index)] inv_index: u32,
) {
    eval(input, output).run_gpu_uniform(inv_index, WG_SIZE, input, output);
}
