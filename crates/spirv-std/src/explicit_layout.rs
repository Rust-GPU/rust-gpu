use crate::arch::IndexUnchecked;
use core::ops::BitOrAssign;

pub(crate) mod sealed_u32_array {
    pub trait Sealed {}
}

/// Implemented by `[u32; N]` for any `N`.
///
/// Workaround to const generics not being stable and allowing `[T; Self::ASSOC_CONST]`, similar to `SmallVec::Array`.
pub unsafe trait U32Array: sealed_u32_array::Sealed {
    const N: usize;

    fn zeroed() -> Self;

    fn as_slice2(&self) -> &[u32];

    fn as_mut_slice2(&mut self) -> &mut [u32];
}

impl<const N: usize> sealed_u32_array::Sealed for [u32; N] {}

unsafe impl<const N: usize> U32Array for [u32; N] {
    const N: usize = N;

    #[inline]
    fn zeroed() -> Self {
        [0; N]
    }

    fn as_slice2(&self) -> &[u32] {
        self.as_slice()
    }

    fn as_mut_slice2(&mut self) -> &mut [u32] {
        self.as_mut_slice()
    }
}

/// This type has a SPIR-V "explicit layout" and may be written to or read from a buffer, push constants, or be used in
/// any other form of structured data access. Since is implemented in pure rust and does not rely on SPIR-V primitive,
/// you may use these functions from the CPU as well to encode or decode your data.
///
/// Requires:
/// * all struct members implement `ExplicitLayout`
/// * type is `#[repr(C)]`
///
/// Non-Requirements:
/// * type can be any alignment, the resulting `[u32; N]` will align the type to at least 4 bytes
pub unsafe trait ExplicitLayout: Sized {
    /// Aligned size is the size of the struct aligned to 4 bytes.
    type ARRAY: U32Array;

    fn read(slice: &[u32], offset: usize) -> Self {
        let mut array = <Self::ARRAY as U32Array>::zeroed();
        slice_copy(
            slice,
            offset,
            array.as_mut_slice2(),
            0,
            <Self::ARRAY as U32Array>::N,
        );
        Self::decode(array)
    }

    fn decode(buffer: Self::ARRAY) -> Self {
        Self::decode_inner(buffer, 0)
    }

    fn decode_inner(buffer: Self::ARRAY, byte_offset: usize) -> Self;

    fn write(slice: &mut [u32], offset: usize, this: Self) {
        let array = Self::encode(this);
        slice_copy(
            array.as_slice2(),
            0,
            slice,
            offset,
            <Self::ARRAY as U32Array>::N,
        );
    }

    fn encode(this: Self) -> Self::ARRAY {
        Self::encode_inner(this, 0)
    }

    fn encode_inner(this: Self, byte_offset: usize) -> Self::ARRAY;
}

/// A rust-gpu friendly `BitOrAssign` on all elements declared by params.
#[inline]
pub fn slice_or<T: Copy + BitOrAssign>(
    src: &[T],
    src_offset: usize,
    dst: &mut [T],
    dst_offset: usize,
    len: usize,
) {
    assert!((src_offset + len) <= src.len());
    assert!((dst_offset + len) <= dst.len());
    unsafe {
        slice_or_unchecked(src, src_offset, dst, dst_offset, len);
    }
}

/// A rust-gpu friendly `BitOrAssign` on all elements declared by params.
#[inline]
pub unsafe fn slice_or_unchecked<T: Copy + BitOrAssign>(
    src: &[T],
    src_offset: usize,
    dst: &mut [T],
    dst_offset: usize,
    len: usize,
) {
    // TODO(@firestar99) should check that this compiles to as few instructions as possible
    unsafe {
        for i in 0..len {
            *dst.index_unchecked_mut(dst_offset + i) |= *src.index_unchecked(src_offset + i);
        }
    }
}

/// A rust-gpu friendly slice copy. To be deprecated when qptr is implemented.
#[inline]
pub fn slice_copy<T: Copy + BitOrAssign>(
    src: &[T],
    src_offset: usize,
    dst: &mut [T],
    dst_offset: usize,
    len: usize,
) {
    assert!((src_offset + len) <= src.len());
    assert!((dst_offset + len) <= dst.len());
    unsafe {
        slice_copy_unchecked(src, src_offset, dst, dst_offset, len);
    }
}

/// A rust-gpu friendly slice copy. To be deprecated when qptr is implemented.
#[inline]
pub unsafe fn slice_copy_unchecked<T: Copy + BitOrAssign>(
    src: &[T],
    src_offset: usize,
    dst: &mut [T],
    dst_offset: usize,
    len: usize,
) {
    // TODO(@firestar99) should check that this compiles to as few instructions as possible
    unsafe {
        for i in 0..len {
            *dst.index_unchecked_mut(dst_offset + i) = *src.index_unchecked(src_offset + i);
        }
    }
}

pub mod prototype {
    use crate::{ExplicitLayout, U32Array, slice_or};
    use core::mem::offset_of;
    use glam::Vec2;

    #[repr(C)]
    #[derive(Copy, Clone, Debug, PartialEq)]
    pub struct MyStruct {
        a: u32,
        b: Vec2,
        c: u8,
        d: u16,
    }

    unsafe impl ExplicitLayout for u32 {
        type ARRAY = [u32; 1];

        #[inline]
        fn decode_inner(buffer: Self::ARRAY, byte_offset: usize) -> Self {
            debug_assert_eq!(byte_offset, 0);
            buffer[0]
        }

        fn encode_inner(this: Self, byte_offset: usize) -> Self::ARRAY {
            debug_assert_eq!(byte_offset, 0);
            [this]
        }
    }

    unsafe impl ExplicitLayout for f32 {
        type ARRAY = [u32; 1];

        #[inline]
        fn decode_inner(buffer: Self::ARRAY, byte_offset: usize) -> Self {
            debug_assert_eq!(byte_offset, 0);
            f32::from_bits(buffer[0])
        }

        fn encode_inner(this: Self, byte_offset: usize) -> Self::ARRAY {
            debug_assert_eq!(byte_offset, 0);
            [f32::to_bits(this)]
        }
    }

    // dirty, should use f32 decode
    unsafe impl ExplicitLayout for Vec2 {
        type ARRAY = [u32; 2];

        #[inline]
        fn decode_inner(buffer: Self::ARRAY, byte_offset: usize) -> Self {
            debug_assert_eq!(byte_offset, 0);
            Vec2::new(f32::from_bits(buffer[0]), f32::from_bits(buffer[1]))
        }

        fn encode_inner(this: Self, byte_offset: usize) -> Self::ARRAY {
            debug_assert_eq!(byte_offset, 0);
            [f32::to_bits(this.x), f32::to_bits(this.y)]
        }
    }

    unsafe impl ExplicitLayout for u16 {
        type ARRAY = [u32; 1];

        #[inline]
        fn decode_inner(buffer: Self::ARRAY, byte_offset: usize) -> Self {
            debug_assert_eq!(byte_offset % align_of::<Self>(), 0);
            (buffer[0] >> (byte_offset << 3)) as u16
        }

        fn encode_inner(this: Self, byte_offset: usize) -> Self::ARRAY {
            debug_assert_eq!(byte_offset % align_of::<Self>(), 0);
            [(this as u32) << (byte_offset << 3)]
        }
    }

    unsafe impl ExplicitLayout for u8 {
        type ARRAY = [u32; 1];

        #[inline]
        fn decode_inner(buffer: Self::ARRAY, byte_offset: usize) -> Self {
            debug_assert_eq!(byte_offset % align_of::<Self>(), 0);
            (buffer[0] >> (byte_offset << 3)) as u8
        }

        fn encode_inner(this: Self, byte_offset: usize) -> Self::ARRAY {
            debug_assert_eq!(byte_offset % align_of::<Self>(), 0);
            [(this as u32) << (byte_offset << 3)]
        }
    }

    unsafe impl ExplicitLayout for MyStruct {
        type ARRAY = [u32; size_of::<MyStruct>().div_ceil(4)];

        fn decode_inner(buffer: Self::ARRAY, byte_offset: usize) -> Self {
            debug_assert_eq!(byte_offset % align_of::<Self>(), 0);
            Self {
                a: {
                    let mut tmp = <u32 as ExplicitLayout>::ARRAY::zeroed();
                    slice_or(
                        &buffer,
                        offset_of!(Self, a) << 2,
                        tmp.as_mut_slice(),
                        0,
                        <<u32 as ExplicitLayout>::ARRAY as U32Array>::N,
                    );
                    <u32 as ExplicitLayout>::decode_inner(tmp, offset_of!(Self, a) & 3)
                },
                b: {
                    let mut tmp = <Vec2 as ExplicitLayout>::ARRAY::zeroed();
                    slice_or(
                        &buffer,
                        offset_of!(Self, b) / 4,
                        tmp.as_mut_slice(),
                        0,
                        <<Vec2 as ExplicitLayout>::ARRAY as U32Array>::N,
                    );
                    <Vec2 as ExplicitLayout>::decode_inner(tmp, offset_of!(Self, b) & 3)
                },
                c: {
                    let mut tmp = <u8 as ExplicitLayout>::ARRAY::zeroed();
                    slice_or(
                        &buffer,
                        offset_of!(Self, c) / 4,
                        tmp.as_mut_slice(),
                        0,
                        <<u8 as ExplicitLayout>::ARRAY as U32Array>::N,
                    );
                    <u8 as ExplicitLayout>::decode_inner(tmp, offset_of!(Self, c) & 3)
                },
                d: {
                    let mut tmp = <u16 as ExplicitLayout>::ARRAY::zeroed();
                    slice_or(
                        &buffer,
                        offset_of!(Self, d) / 4,
                        tmp.as_mut_slice(),
                        0,
                        <<u16 as ExplicitLayout>::ARRAY as U32Array>::N,
                    );
                    <u16 as ExplicitLayout>::decode_inner(tmp, offset_of!(Self, d) & 3)
                },
            }
        }

        fn encode_inner(this: Self, byte_offset: usize) -> Self::ARRAY {
            debug_assert_eq!(byte_offset % align_of::<Self>(), 0);
            let mut out = Self::ARRAY::zeroed();
            {
                let tmp = u32::encode_inner(this.a, offset_of!(Self, a) & 3);
                slice_or(
                    &tmp,
                    0,
                    out.as_mut_slice(),
                    offset_of!(Self, a) / 4,
                    <<u32 as ExplicitLayout>::ARRAY as U32Array>::N,
                );
            }
            {
                let tmp = Vec2::encode_inner(this.b, offset_of!(Self, b) & 3);
                slice_or(
                    &tmp,
                    0,
                    out.as_mut_slice(),
                    offset_of!(Self, b) / 4,
                    <<Vec2 as ExplicitLayout>::ARRAY as U32Array>::N,
                );
            }
            {
                let tmp = u8::encode_inner(this.c, offset_of!(Self, c) & 3);
                slice_or(
                    &tmp,
                    0,
                    out.as_mut_slice(),
                    offset_of!(Self, c) / 4,
                    <<u8 as ExplicitLayout>::ARRAY as U32Array>::N,
                );
            }
            {
                let tmp = u16::encode_inner(this.d, offset_of!(Self, d) & 3);
                slice_or(
                    &tmp,
                    0,
                    out.as_mut_slice(),
                    offset_of!(Self, d) / 4,
                    <<u16 as ExplicitLayout>::ARRAY as U32Array>::N,
                );
            }
            out
        }
    }

    #[test]
    fn test_roundtrip() {
        let this = MyStruct {
            a: 42,
            b: Vec2::new(-1., 0.345),
            c: 255,
            d: 69,
        };
        assert_eq!(MyStruct::decode(MyStruct::encode(this)), this);
    }
}
