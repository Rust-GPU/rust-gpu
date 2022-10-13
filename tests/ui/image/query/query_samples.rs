// build-pass
// compile-flags: -C target-feature=+ImageQuery

use spirv_std::{arch, Image};

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled, multisampled),
    #[rust_gpu::spirv(flat)] output: &mut u32,
) {
    *output = image.query_samples();
}
