use spirv_std::glam::*;

pub const OUT_LEN: usize = 1;

pub fn eval(_gid: u32, out: &mut [u32]) {
    out[0] = 42;
}
