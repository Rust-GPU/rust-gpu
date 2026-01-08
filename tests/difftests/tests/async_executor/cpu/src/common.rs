use core::ops::Range;
use spirv_std::glam::*;

pub const OUT_LEN: usize = 128;

#[cfg(not(target_arch = "spirv"))]
pub fn input_dataset() -> Vec<u32> {
    (0..OUT_LEN).map(|i| i as u32).collect()
}

pub fn eval(_input: &[u32], _out: &mut [u32]) -> TaskBlock<impl BlockFn> {
    TaskBlock::new(0..78, |i, input, output| {
        output[i as usize] = input[i as usize];
    })
}

pub trait BlockFn: Fn(u32, &[u32], &mut [u32]) {}

impl<T: Fn(u32, &[u32], &mut [u32])> BlockFn for T {}

pub struct TaskBlock<F: BlockFn> {
    range: Range<u32>,
    closure: F,
}

impl<F: BlockFn> TaskBlock<F> {
    pub fn new(range: Range<u32>, closure: F) -> Self {
        Self { range, closure }
    }

    pub fn run_cpu(&self, input: &[u32], output: &mut [u32]) {
        for i in self.range.clone() {
            (self.closure)(i, input, output);
        }
    }

    pub fn run_gpu_uniform(&self, inv_index: u32, sg_size: u32, input: &[u32], output: &mut [u32]) {
        let item_cnt = self.range.start - self.range.end;
        for group in (0..item_cnt).step_by(sg_size as usize) {
            let i = self.range.start + group + inv_index;
            (self.closure)(i, input, output);
        }
        let remainder = item_cnt % sg_size;
        if inv_index < remainder {
            let i = (item_cnt / sg_size) + inv_index;
            (self.closure)(i, input, output);
        }
    }
}
