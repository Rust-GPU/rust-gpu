// Test all trailing and leading zeros. No need to test ones, they just call the zero variant with !value

// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn leading_zeros_u32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u32,
    out: &mut u32,
) {
    *out = u32::leading_zeros(*buffer);
}

#[spirv(fragment)]
pub fn trailing_zeros_u32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &u32,
    out: &mut u32,
) {
    *out = u32::trailing_zeros(*buffer);
}

#[spirv(fragment)]
pub fn leading_zeros_i32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i32,
    out: &mut u32,
) {
    *out = i32::leading_zeros(*buffer);
}

#[spirv(fragment)]
pub fn trailing_zeros_i32(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buffer: &i32,
    out: &mut u32,
) {
    *out = i32::trailing_zeros(*buffer);
}
