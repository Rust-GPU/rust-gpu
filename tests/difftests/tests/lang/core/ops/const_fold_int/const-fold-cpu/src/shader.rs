use crate::{EvalResult, Variants};
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] variant: &u32,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] input_patterns: &[u32; 8],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 2)] output: &mut EvalResult,
) {
    if let Ok(variant) = Variants::try_from(*variant) {
        *output = variant.eval(input_patterns);
    }
}
