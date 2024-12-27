// build-fail
#![allow(unconditional_panic)]

#![cfg_attr(target_arch = "spirv", no_std)]
use spirv_std::spirv;

// note that &mut [usize; 0] will cause an even worse panic
#[spirv(compute(threads(1, 1, 1)))]
pub fn compute(m: [usize; 0]) {}
