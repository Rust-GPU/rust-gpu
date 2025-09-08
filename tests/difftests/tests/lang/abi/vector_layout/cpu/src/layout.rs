use bytemuck::{Pod, Zeroable};
use spirv_std::glam::*;

#[repr(C)]
#[derive(Copy, Clone, Zeroable, Pod)]
pub struct SizeAndAlign {
    pub size: u32,
    pub align: u32,
}

impl SizeAndAlign {
    pub fn from<T>() -> Self {
        Self {
            size: size_of::<T>() as u32,
            align: align_of::<T>() as u32,
        }
    }

    pub fn flatten(&self) -> [u32; 2] {
        [self.size, self.align]
    }
}

pub type EvalResult = [SizeAndAlign; 8];

pub fn eval_layouts() -> EvalResult {
    [
        SizeAndAlign::from::<u32>(),
        SizeAndAlign::from::<UVec2>(),
        SizeAndAlign::from::<UVec3>(),
        SizeAndAlign::from::<UVec4>(),
        SizeAndAlign::from::<f32>(),
        SizeAndAlign::from::<Vec2>(),
        SizeAndAlign::from::<Vec3>(),
        SizeAndAlign::from::<Vec4>(),
    ]
}
