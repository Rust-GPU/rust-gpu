// Tests that checked / overflowing / unchecked multiplication compile, including
// the `unchecked_mul` precondition check that internally calls `overflowing_mul`
// (see https://github.com/Rust-GPU/rust-gpu/issues/537).

// build-pass
// compile-flags: -C target-feature=+Int8,+Int16,+Int64

use spirv_std::spirv;

#[spirv(fragment)]
pub fn checked_mul_u8(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &u8,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &u8,
    out: &mut u32,
) {
    let (r, o) = a.overflowing_mul(*b);
    *out = (r as u32) | ((o as u32) << 8);
    if let Some(v) = a.checked_mul(*b) {
        *out ^= v as u32;
    }
}

#[spirv(fragment)]
pub fn checked_mul_u16(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &u16,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &u16,
    out: &mut u32,
) {
    let (r, o) = a.overflowing_mul(*b);
    *out = (r as u32) | ((o as u32) << 16);
    if let Some(v) = a.checked_mul(*b) {
        *out ^= v as u32;
    }
}

#[spirv(fragment)]
pub fn checked_mul_u32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &u32,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &u32,
    out: &mut u32,
) {
    let (r, o) = a.overflowing_mul(*b);
    *out = r ^ (o as u32);
    if let Some(v) = a.checked_mul(*b) {
        *out ^= v;
    }
}

#[spirv(fragment)]
pub fn checked_mul_u64(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &u64,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &u64,
    out: &mut u32,
) {
    let (r, o) = a.overflowing_mul(*b);
    *out = (r as u32) ^ (o as u32);
    if let Some(v) = a.checked_mul(*b) {
        *out ^= v as u32;
    }
}

#[spirv(fragment)]
pub fn checked_mul_i8(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &i8,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &i8,
    out: &mut u32,
) {
    let (r, o) = a.overflowing_mul(*b);
    *out = (r as u32) ^ ((o as u32) << 8);
    if let Some(v) = a.checked_mul(*b) {
        *out ^= v as u32;
    }
}

#[spirv(fragment)]
pub fn checked_mul_i16(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &i16,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &i16,
    out: &mut u32,
) {
    let (r, o) = a.overflowing_mul(*b);
    *out = (r as u32) ^ ((o as u32) << 16);
    if let Some(v) = a.checked_mul(*b) {
        *out ^= v as u32;
    }
}

#[spirv(fragment)]
pub fn checked_mul_i32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &i32,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &i32,
    out: &mut u32,
) {
    let (r, o) = a.overflowing_mul(*b);
    *out = (r as u32) ^ (o as u32);
    if let Some(v) = a.checked_mul(*b) {
        *out ^= v as u32;
    }
}

#[spirv(fragment)]
pub fn checked_mul_i64(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &i64,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &i64,
    out: &mut u32,
) {
    let (r, o) = a.overflowing_mul(*b);
    *out = (r as u32) ^ (o as u32);
    if let Some(v) = a.checked_mul(*b) {
        *out ^= v as u32;
    }
}

// Issue #537 specifically: `unchecked_mul`'s precondition check uses
// `overflowing_mul`, which previously zombied with "checked mul is not
// supported yet".
#[spirv(fragment)]
pub fn unchecked_mul_u32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &u32,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &u32,
    out: &mut u32,
) {
    *out = unsafe { a.unchecked_mul(*b) };
}

#[spirv(fragment)]
pub fn unchecked_mul_i32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &i32,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &i32,
    out: &mut u32,
) {
    *out = unsafe { a.unchecked_mul(*b) } as u32;
}

// The original issue used `usize::unchecked_mul()` (e.g. via `Layout::repeat`).
#[spirv(fragment)]
pub fn unchecked_mul_usize(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &u32,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &u32,
    out: &mut u32,
) {
    let a = *a as usize;
    let b = *b as usize;
    *out = unsafe { a.unchecked_mul(b) } as u32;
}

#[spirv(fragment)]
pub fn checked_mul_usize(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &u32,
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &u32,
    out: &mut u32,
) {
    let a = *a as usize;
    let b = *b as usize;
    let (r, o) = a.overflowing_mul(b);
    *out = (r as u32) ^ (o as u32);
    if let Some(v) = a.checked_mul(b) {
        *out ^= v as u32;
    }
}
