#![cfg_attr(target_arch = "spirv", no_std)]
use spirv_std::spirv;

fn example<const LENGTH: usize>() {
    let mut array = [0; LENGTH];
    for i in 0..array.len() {
        array[i] += i;
    }
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn compute() {
    example::<0>();
}
