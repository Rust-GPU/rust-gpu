// build-pass

use spirv_std as _;

#[rust_gpu::spirv(fragment)]
pub fn main() {
    let arr = [0u32; 32];
}