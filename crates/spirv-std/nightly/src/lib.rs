#![no_std]
#![feature(adt_const_params)]
#![cfg_attr(target_arch = "spirv", feature(asm_experimental_arch))]

pub mod arch;
pub use spirv_std::*;
