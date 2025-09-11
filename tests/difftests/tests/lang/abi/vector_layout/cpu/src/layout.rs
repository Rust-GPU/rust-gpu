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
pub const LAYOUT_COUNT: usize = 0x40;
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

        // experiments structs
        0x10 => write_layout!(out, offset, Struct0x10(a, b)),
        0x11 => write_layout!(out, offset, Struct0x11(a, b)),
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
}
