// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"

use core::num::NonZeroU32;
use spirv_std::glam::Vec2;
use spirv_std::spirv;

#[spirv_std::spirv_vector]
#[derive(Copy, Clone, Default)]
pub struct ZstVector;

#[spirv_std::spirv_vector]
#[derive(Copy, Clone, Default)]
pub struct NotVectorField {
    _x: Vec2,
    _y: Vec2,
}

#[spirv_std::spirv_vector]
#[derive(Copy, Clone)]
pub struct NotVectorField2 {
    _x: NonZeroU32,
    _y: NonZeroU32,
}

impl Default for NotVectorField2 {
    fn default() -> Self {
        Self {
            _x: NonZeroU32::new(1).unwrap(),
            _y: NonZeroU32::new(1).unwrap(),
        }
    }
}

#[spirv(fragment)]
pub fn entry(
    // workaround to ZST loading
    #[spirv(storage_class, descriptor_set = 0, binding = 0)] _: &(ZstVector, i32),
    _: NotVectorField,
    #[spirv(flat)] _: NotVectorField2,
) {
}
