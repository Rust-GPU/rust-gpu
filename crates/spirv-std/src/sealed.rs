/// A marker trait used to prevent other traits from being implemented outside
/// of `spirv-std`.
pub trait Sealed {}

impl Sealed for bool {}
impl Sealed for f32 {}
impl Sealed for f64 {}
impl Sealed for u8 {}
impl Sealed for u16 {}
impl Sealed for u32 {}
impl Sealed for u64 {}
impl Sealed for i8 {}
impl Sealed for i16 {}
impl Sealed for i32 {}
impl Sealed for i64 {}

impl Sealed for glam::Vec2 {}
impl Sealed for glam::Vec3 {}
impl Sealed for glam::Vec4 {}
impl Sealed for glam::DVec2 {}
impl Sealed for glam::DVec3 {}
impl Sealed for glam::DVec4 {}
impl Sealed for glam::UVec2 {}
impl Sealed for glam::UVec3 {}
impl Sealed for glam::UVec4 {}
impl Sealed for glam::IVec2 {}
impl Sealed for glam::IVec3 {}
impl Sealed for glam::IVec4 {}

impl Sealed for glam::Vec3A {}
