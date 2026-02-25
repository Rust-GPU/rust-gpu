// build-pass
// compile-flags: -Ctarget-feature=+Int64,+ShaderClockKHR,+ext:SPV_KHR_shader_clock

use glam::UVec2;
use spirv_std::spirv;
use spirv_std::{
    memory::Scope,
    shader_clock::{read_clock, read_clock_uvec2},
};

#[spirv(fragment)]
pub fn main(out: &mut u32) {
    unsafe {
        let clock_time: u64 = read_clock();
        let clock_time_uvec2: UVec2 = read_clock_uvec2();
        *out = clock_time as u32 + clock_time_uvec2.x;
    }
}
