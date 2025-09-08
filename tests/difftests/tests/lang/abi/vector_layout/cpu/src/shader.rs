use crate::layout::{EvalResult, eval_layouts};
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main_cs(#[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut EvalResult) {
    *output = eval_layouts();
}
