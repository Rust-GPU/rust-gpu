#![allow(unconditional_panic)]

#![cfg_attr(target_arch = "spirv", no_std)]
use spirv_std::spirv;

#[spirv(compute(threads(1, 1, 1)))]
pub fn compute() {
    let mut array = [0; 0];
    // for some reason this now compiles?
    array[0] = 1;
}
