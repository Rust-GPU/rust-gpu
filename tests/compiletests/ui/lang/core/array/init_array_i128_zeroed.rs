// build-pass

use spirv_std::spirv;

/// `crypto-common` has a proc macro to implement code for u8, u16, u32, u16 and u128,
/// which means we need our codegen to handle the u128 case for the crate to even compile.
/// Specifically, this calls `memset_const_pattern` with u128.
///
/// <https://github.com/RustCrypto/traits/blob/d9067c494b4ae8f1ecbc9ae72b683e8a65dd53ce/crypto-common/src/hazmat.rs#L347-L363>
///
/// Note that the u128 path is unusable anyway as 128-bit integer are not supported in SPIR-V.
/// So if this is used nowhere in any entry point, our post-link DCE will remove the u128 code,
/// and the SPIR-V will be valid.
pub fn u128_zero_fill() -> [u128; 4] {
    return [0; 4];
}

pub fn i128_zero_fill() -> [i128; 4] {
    return [0; 4];
}

#[spirv(fragment)]
pub fn main() {}
