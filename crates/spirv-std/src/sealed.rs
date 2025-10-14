/// A marker trait used to prevent other traits from being implemented outside
/// of `spirv-std`.
pub trait Sealed {}

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
