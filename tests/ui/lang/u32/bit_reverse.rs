// Test all trailing and leading zeros. No need to test ones, they just call the zero variant with !value

// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn count_zeros_u8(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u8,
    out: &mut u8,
) {
    *out = u8::reverse_bits(*buffer);
}

#[spirv(fragment)]
pub fn count_zeros_u16(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u16,
    out: &mut u16,
) {
    *out = u16::reverse_bits(*buffer);
}

#[spirv(fragment)]
pub fn count_zeros_u32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u32,
    out: &mut u32,
) {
    *out = u32::reverse_bits(*buffer);
}

#[spirv(fragment)]
pub fn count_zeros_u64(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u64,
    out: &mut u64,
) {
    *out = u64::reverse_bits(*buffer);
}

#[spirv(fragment)]
pub fn count_zeros_i8(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i8,
    out: &mut i8,
) {
    *out = i8::reverse_bits(*buffer);
}

#[spirv(fragment)]
pub fn count_zeros_i16(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i16,
    out: &mut i16,
) {
    *out = i16::reverse_bits(*buffer);
}

#[spirv(fragment)]
pub fn count_zeros_i32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i32,
    out: &mut i32,
) {
    *out = i32::reverse_bits(*buffer);
}

#[spirv(fragment)]
pub fn count_zeros_i64(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i64,
    out: &mut i64,
) {
    *out = i64::reverse_bits(*buffer);
}
