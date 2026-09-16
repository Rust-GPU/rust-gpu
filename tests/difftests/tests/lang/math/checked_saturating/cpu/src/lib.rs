#![cfg_attr(target_arch = "spirv", no_std)]

use spirv_std::spirv;

pub trait ToU32 {
    fn to_u32(&self) -> u32;
}

impl ToU32 for u32 {
    fn to_u32(&self) -> u32 {
        *self
    }
}

impl ToU32 for i32 {
    fn to_u32(&self) -> u32 {
        *self as u32
    }
}

#[inline]
pub fn ch<T: ToU32>(opt: Option<T>) -> [u32; 2] {
    match opt {
        None => [u32::default(), 1],
        Some(e) => [e.to_u32(), 0],
    }
}

#[inline]
pub fn ov<T: ToU32>((value, overflow): (T, bool)) -> [u32; 2] {
    [value.to_u32(), overflow.into()]
}

pub const LEN: usize = 24;
pub type Output = [[u32; 2]; LEN];

pub fn eval() -> Output {
    [
        // u32
        ov(1u32.overflowing_add(1)),
        ch(1u32.checked_add(1)),
        ov(u32::MAX.overflowing_add(1)),
        ch(u32::MAX.checked_add(1)),
        ch(0u32.checked_add(0)),
        ov(0u32.overflowing_add(0)),
        ov(1u32.overflowing_sub(1)),
        ch(1u32.checked_sub(1)),
        ov(u32::MIN.overflowing_sub(1)),
        ch(u32::MIN.checked_sub(1)),
        ch(0u32.checked_sub(0)),
        ov(0u32.overflowing_sub(0)),
        // i32
        ov(1i32.overflowing_add(1)),
        ch(1i32.checked_add(1)),
        ov(i32::MAX.overflowing_add(1)),
        ch(i32::MAX.checked_add(1)),
        ch(0i32.checked_add(0)),
        ov(0i32.overflowing_add(0)),
        ov(1i32.overflowing_sub(1)),
        ch(1i32.checked_sub(1)),
        ov(i32::MIN.overflowing_sub(1)),
        ch(i32::MIN.checked_sub(1)),
        ch(0i32.checked_sub(0)),
        ov(0i32.overflowing_sub(0)),
    ]
}

#[spirv(compute(threads(1)))]
pub fn main_cs(#[spirv(storage_buffer, descriptor_set = 0, binding = 0)] out: &mut Output) {
    *out = eval();
}

#[cfg(not(target_arch = "spirv"))]
mod cpu {
    use super::*;
    use difftest::config::Config;
    use difftest::scaffold::compute::{BufferConfig, WgpuComputeTest};
    use difftest::scaffold::shader::RustComputeShader;

    pub fn cpu_driver() {
        let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
        config.write_result(&eval()).unwrap()
    }

    pub fn shader_driver() {
        let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
        if cfg!(target_os = "macos") {
            let skip = difftest::scaffold::Skip::new("no Vulkan on MacOS");
            skip.run_test(&config).unwrap();
            return;
        }

        let test = WgpuComputeTest::new(
            RustComputeShader::default().passthrough(),
            [1, 1, 1],
            Vec::from(&[BufferConfig::writeback(size_of::<Output>())]),
        );
        test.run_test(&config).unwrap();
    }
}

#[cfg(not(target_arch = "spirv"))]
pub use cpu::*;
