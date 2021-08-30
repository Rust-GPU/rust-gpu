// build-pass
// compile-flags: -Ctarget-feature=+RayTracingKHR,+ext:SPV_KHR_ray_tracing

use spirv_std as _;

#[derive(Clone, Copy)]
#[spirv(matrix)]
pub struct Affine3 {
    pub x: glam::Vec3,
    pub y: glam::Vec3,
    pub z: glam::Vec3,
    pub w: glam::Vec3,
}

impl Affine3 {
    pub const ZERO: Self = Self {
        x: glam::Vec3::ZERO,
        y: glam::Vec3::ZERO,
        z: glam::Vec3::ZERO,
        w: glam::Vec3::ZERO,
    };

    pub const IDENTITY: Self = Self {
        x: glam::Vec3::X,
        y: glam::Vec3::Y,
        z: glam::Vec3::Z,
        w: glam::Vec3::ZERO,
    };
}

impl Default for Affine3 {
    #[inline]
    fn default() -> Self {
        Self::IDENTITY
    }
}

#[spirv(closest_hit)]
pub fn main_attrs(
    #[spirv(object_to_world)] _object_to_world: Affine3,
    #[spirv(world_to_object)] _world_to_object: Affine3,
) {
}

#[spirv(closest_hit)]
pub fn main_default(out: &mut Affine3) {
    *out = Affine3::default();
}

#[spirv(closest_hit)]
pub fn main_add(
    #[spirv(object_to_world)] object_to_world: Affine3,
    #[spirv(world_to_object)] world_to_object: Affine3,
    out: &mut glam::Vec3,
) {
    *out = object_to_world.x
        + object_to_world.y
        + object_to_world.z
        + object_to_world.w
        + world_to_object.x
        + world_to_object.y
        + world_to_object.z
        + world_to_object.w;
}
