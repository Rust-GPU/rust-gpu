// build-fail

#![cfg_attr(target_arch = "spirv", no_std)]
use spirv_std::spirv;

fn example<const N: usize>() {
    let mut array = [0; N];
    for i in 0..N {
        array[i] += i;
    }
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn compute() {
    example::<0>();
}
