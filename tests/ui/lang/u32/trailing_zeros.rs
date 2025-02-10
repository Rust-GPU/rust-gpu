// Test all trailing and leading zeros. No need to test ones, they just call the zero variant with !value

// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn trailing_zeros_u8(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u8,
    out: &mut u32,
) {
    *out = u8::trailing_zeros(*buffer);
}

#[spirv(fragment)]
pub fn trailing_zeros_u16(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u16,
    out: &mut u32,
) {
    *out = u16::trailing_zeros(*buffer);
}

#[spirv(fragment)]
pub fn trailing_zeros_u32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u32,
    out: &mut u32,
) {
    *out = u32::trailing_zeros(*buffer);
}

#[spirv(fragment)]
pub fn trailing_zeros_u64(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u64,
    out: &mut u32,
) {
    *out = u64::trailing_zeros(*buffer);
}

#[spirv(fragment)]
pub fn trailing_zeros_i8(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i8,
    out: &mut u32,
) {
    *out = i8::trailing_zeros(*buffer);
}

#[spirv(fragment)]
pub fn trailing_zeros_i16(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i16,
    out: &mut u32,
) {
    *out = i16::trailing_zeros(*buffer);
}

#[spirv(fragment)]
pub fn trailing_zeros_i32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i32,
    out: &mut u32,
) {
    *out = i32::trailing_zeros(*buffer);
}

#[spirv(fragment)]
pub fn trailing_zeros_i64(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i64,
    out: &mut u32,
) {
    *out = i64::trailing_zeros(*buffer);
}
