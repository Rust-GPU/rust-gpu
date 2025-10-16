// build-fail
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use core::num::NonZeroU32;
use spirv_std::glam::Vec2;
use spirv_std::spirv;

#[rust_gpu::vector::v1]
pub struct FewerFields {
    _v: f32,
}

#[rust_gpu::vector::v1]
pub struct TooManyFields {
    _x: f32,
    _y: f32,
    _z: f32,
    _w: f32,
    _v: f32,
}

#[rust_gpu::vector::v1]
pub struct NotVectorField {
    _x: Vec2,
    _y: Vec2,
}

#[rust_gpu::vector::v1]
pub struct NotVectorField2 {
    _x: NonZeroU32,
    _y: NonZeroU32,
}

#[rust_gpu::vector::v1]
pub struct DifferentTypes {
    _x: f32,
    _y: u32,
}

#[spirv(fragment)]
pub fn entry(
    _: FewerFields,
    _: TooManyFields,
    _: NotVectorField,
    #[spirv(flat)] _: NotVectorField2,
    #[spirv(flat)] _: DifferentTypes,
) {
}
