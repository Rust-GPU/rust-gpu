// build-fail

use core::num::NonZeroU32;
use spirv_std::glam::Vec2;
use spirv_std::spirv;

#[spirv_std::spirv_vector]
#[derive(Copy, Clone, Default)]
pub struct FewerFields {
    _v: f32,
}

#[spirv_std::spirv_vector]
#[derive(Copy, Clone, Default)]
pub struct TooManyFields {
    _x: f32,
    _y: f32,
    _z: f32,
    _w: f32,
    _v: f32,
}

// wrong member types fails too early

#[spirv_std::spirv_vector]
#[derive(Copy, Clone, Default)]
pub struct DifferentTypes {
    _x: f32,
    _y: u32,
}

#[spirv(fragment)]
pub fn entry(_: FewerFields, _: TooManyFields, #[spirv(flat)] _: DifferentTypes) {}
