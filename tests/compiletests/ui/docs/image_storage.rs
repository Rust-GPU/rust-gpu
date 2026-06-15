// built-pass
// compile-flags: -C target-feature=+StorageImageWriteWithoutFormat

use spirv_std::Sampler;
use spirv_std::glam::*;
use spirv_std::image::*;
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(descriptor_set = 0, binding = 0)] storage_image: &StorageImage2d) {
    let pixel = UVec2::new(123, 456);
    let color = Vec4::new(0.3, 0.4, 0.5, 0.6);
    unsafe {
        storage_image.write(pixel, color);
    }
}
