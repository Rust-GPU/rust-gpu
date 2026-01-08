// build-pass

use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main() {
    async {};
}
