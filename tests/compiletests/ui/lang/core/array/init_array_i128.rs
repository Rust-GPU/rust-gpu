// build-pass

use spirv_std::spirv;

pub fn u128_constant() -> [u128; 4] {
    return [0x123456789ABCDEF0123456789ABCDEF; 4];
}

pub fn i128_constant() -> [i128; 4] {
    return [0x123456789ABCDEF0123456789ABCDEF; 4];
}

#[spirv(fragment)]
pub fn main() {}
