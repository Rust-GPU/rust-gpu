use core::mem::offset_of;
use experiments::*;
use spirv_std::glam::*;

pub struct BumpAlloc(usize);

impl BumpAlloc {
    pub fn inc(&mut self) -> usize {
        let old = self.0;
        self.0 += 1;
        old
    }
}

pub trait WriteLayout {
    fn write_layout(offset: &mut BumpAlloc, out: &mut [u32]);
}

macro_rules! write_layout {
    ($out:ident, $offset:ident, $name:ident($($member:ident),*)) => {
		{
			// offset 0: size
			$out[$offset.inc()] = size_of::<$name>() as u32;
			// offset 4: alignment
			$out[$offset.inc()] = align_of::<$name>() as u32;
			// offset 8 onwards: members
			$($out[$offset.inc()] = offset_of!($name, $member) as u32;)*
		}
	};
}

/// gid is checked between `0..LAYOUT_COUNT`
pub const LAYOUT_COUNT: usize = 0x60;
/// at byte offset 0x100 * N starts layout N
pub const LAYOUT_MAX_SIZE: usize = 0x100 / 0x4;
pub const LAYOUT_LEN: usize = LAYOUT_COUNT * LAYOUT_MAX_SIZE;

pub fn eval_layouts(gid: u32, out: &mut [u32]) {
    let mut offset = BumpAlloc(gid as usize * LAYOUT_MAX_SIZE);
    match gid {
        // vec
        0x0 => write_layout!(out, offset, u32()),
        0x1 => write_layout!(out, offset, i32()),
        0x2 => write_layout!(out, offset, f32()),
        0x4 => write_layout!(out, offset, UVec2(x, y)),
        0x5 => write_layout!(out, offset, IVec2(x, y)),
        0x6 => write_layout!(out, offset, Vec2(x, y)),
        0x8 => write_layout!(out, offset, UVec3(x, y, z)),
        0x9 => write_layout!(out, offset, IVec3(x, y, z)),
        0xA => write_layout!(out, offset, Vec3(x, y, z)),
        0xB => write_layout!(out, offset, Vec3A()), // private members
        0xC => write_layout!(out, offset, UVec4(x, y, z, w)),
        0xD => write_layout!(out, offset, IVec4(x, y, z, w)),
        0xE => write_layout!(out, offset, Vec4()), // private members
        0xF => write_layout!(out, offset, Quat()), // private members

        // experiments structs
        0x10 => write_layout!(out, offset, Struct0x10(a, b)),
        0x11 => write_layout!(out, offset, Struct0x11(a, b)),
        0x12 => write_layout!(out, offset, Struct0x12(a, b, c, d, e)),
        0x13 => write_layout!(out, offset, Struct0x13(a)),

        // mat
        0x20 => write_layout!(out, offset, Mat2()), // private members
        0x21 => write_layout!(out, offset, Mat3(x_axis, y_axis, z_axis)),
        0x22 => write_layout!(out, offset, Mat3A(x_axis, y_axis, z_axis)),
        0x23 => write_layout!(out, offset, Mat4(x_axis, y_axis, z_axis, w_axis)),
        // f64
        0x24 => write_layout!(out, offset, f64()),
        0x25 => write_layout!(out, offset, DVec2(x, y)),
        0x26 => write_layout!(out, offset, DVec3(x, y, z)),
        0x27 => write_layout!(out, offset, DVec4(x, y, z, w)),
        0x28 => write_layout!(out, offset, DQuat(x, y, z, w)),
        0x29 => write_layout!(out, offset, DMat2()), // private members
        0x2a => write_layout!(out, offset, DMat3(x_axis, y_axis, z_axis)),
        0x2b => write_layout!(out, offset, DMat4(x_axis, y_axis, z_axis, w_axis)),

        // i8
        0x30 => write_layout!(out, offset, i8()),
        0x31 => write_layout!(out, offset, I8Vec2(x, y)),
        0x32 => write_layout!(out, offset, I8Vec3(x, y, z)),
        0x33 => write_layout!(out, offset, I8Vec4(x, y, z, w)),
        // i16
        0x34 => write_layout!(out, offset, i16()),
        0x35 => write_layout!(out, offset, I16Vec2(x, y)),
        0x36 => write_layout!(out, offset, I16Vec3(x, y, z)),
        0x37 => write_layout!(out, offset, I16Vec4(x, y, z, w)),
        // i64
        0x38 => write_layout!(out, offset, i64()),
        0x39 => write_layout!(out, offset, I64Vec2(x, y)),
        0x3a => write_layout!(out, offset, I64Vec3(x, y, z)),
        0x3b => write_layout!(out, offset, I64Vec4(x, y, z, w)),

        // u8
        0x40 => write_layout!(out, offset, u8()),
        0x41 => write_layout!(out, offset, U8Vec2(x, y)),
        0x42 => write_layout!(out, offset, U8Vec3(x, y, z)),
        0x43 => write_layout!(out, offset, U8Vec4(x, y, z, w)),
        // u16
        0x44 => write_layout!(out, offset, u16()),
        0x45 => write_layout!(out, offset, U16Vec2(x, y)),
        0x46 => write_layout!(out, offset, U16Vec3(x, y, z)),
        0x47 => write_layout!(out, offset, U16Vec4(x, y, z, w)),
        // u64
        0x48 => write_layout!(out, offset, u64()),
        0x49 => write_layout!(out, offset, U64Vec2(x, y)),
        0x4a => write_layout!(out, offset, U64Vec3(x, y, z)),
        0x4b => write_layout!(out, offset, U64Vec4(x, y, z, w)),
        // Affine
        0x4c => write_layout!(out, offset, Affine2(matrix2, translation)),
        0x4d => write_layout!(out, offset, Affine3A(matrix3, translation)),
        0x4e => write_layout!(out, offset, DAffine2(matrix2, translation)),
        0x4f => write_layout!(out, offset, DAffine3(matrix3, translation)),

        // bool
        0x50 => write_layout!(out, offset, bool()),
        0x51 => write_layout!(out, offset, BVec2(x, y)),
        0x52 => write_layout!(out, offset, BVec3(x, y, z)),
        0x53 => write_layout!(out, offset, BVec3A()), // private members
        0x54 => write_layout!(out, offset, BVec4(x, y, z, w)),
        0x55 => write_layout!(out, offset, BVec4A()),

        _ => {}
    }
}

mod experiments {
    use spirv_std::glam::*;

    #[repr(C)]
    pub struct Struct0x10 {
        pub a: f32,
        /// if UVec2 has an alignment of
        /// * 4 (CPU): 12 size, 4 alignment, 4 b offset
        /// * 8 (GPU): 16 size, 8 alignment, 8 b offset
        pub b: UVec2,
    }

    #[repr(C)]
    pub struct Struct0x11 {
        pub a: Vec3,
        /// if Vec3 has an alignment of
        /// * 4 (CPU): 16 size, 4 alignment, 12 b offset
        /// * 16 (GPU): 32 size, 16 alignment, 16 b offset
        pub b: f32,
    }

    #[repr(C)]
    pub struct Struct0x12 {
        pub a: BVec3,
        pub b: f32,
        pub c: BVec2,
        pub d: f32,
        pub e: BVec4,
    }

    #[repr(C)]
    #[repr(align(16))]
    pub struct Struct0x13 {
        pub a: f32,
    }
}
