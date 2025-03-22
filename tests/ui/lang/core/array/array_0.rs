#![allow(unconditional_panic)]

// build-fail
#![cfg_attr(target_arch = "spirv", no_std)]
use spirv_std::spirv;

#[spirv(compute(threads(1, 1, 1)))]
pub fn compute() {
    let array = [0; 0];
    let x = array[0];
}
