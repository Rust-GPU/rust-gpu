#![cfg_attr(target_arch = "spirv", no_std)]
#![allow(arithmetic_overflow)]

#[cfg(not(target_arch = "spirv"))]
pub mod cpu_driver;
pub mod shader;
#[cfg(not(target_arch = "spirv"))]
pub mod shader_driver;

use num_enum::{IntoPrimitive, TryFromPrimitive};

macro_rules! op_u {
    ($value:expr) => {
        [
            ($value << 0) as u32,
            ($value << 1) as u32,
            ($value << 2) as u32,
            ($value << 30) as u32,
            ($value << 31) as u32,
            ($value << 32) as u32,
            ($value << 33) as u32,
            ($value << 34) as u32,
            ($value >> 0) as u32,
            ($value >> 1) as u32,
            ($value >> 2) as u32,
            ($value >> 30) as u32,
            ($value >> 31) as u32,
            ($value >> 32) as u32,
            ($value >> 33) as u32,
            ($value >> 34) as u32,
        ]
    };
}

macro_rules! op_i {
    ($value:expr) => {
        op_u!($value as i32)
    };
}

macro_rules! interesting_patterns {
    ($op_name:ident) => {
        [
            $op_name!(0u32),
            $op_name!(1u32),
            $op_name!(0xFFFFFFFFu32),
            $op_name!(0xDEADBEEFu32),
            $op_name!(0b10011001100110011001100110011001u32),
            $op_name!(0b10000000000000000000000000000001u32),
            $op_name!(0x12345678u32),
            $op_name!(0x87654321u32),
        ]
    };
}

macro_rules! identity {
    ($expr:expr) => {
        $expr
    };
}

pub const INTERESTING_PATTERNS: [u32; 8] = interesting_patterns!(identity);

#[repr(u32)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, TryFromPrimitive, IntoPrimitive)]
pub enum Variants {
    /// const folding in rust-gpu
    ConstFold,
    // /// `const {}` expr for const eval within rustc
    // ConstExpr,
    /// dynamic values from `input_patterns`
    DynamicValues,
}

pub type EvalResult = [[[u32; 16]; 8]; 2];

impl Variants {
    pub fn eval(&self, input_patterns: &[u32; 8]) -> EvalResult {
        match self {
            Variants::ConstFold => [interesting_patterns!(op_u), interesting_patterns!(op_i)],
            // Variants::ConstExpr => {
            //     const { [interesting_patterns!(op_u), interesting_patterns!(op_i)] }
            // }
            Variants::DynamicValues => [
                [
                    op_u!(input_patterns[0]),
                    op_u!(input_patterns[1]),
                    op_u!(input_patterns[2]),
                    op_u!(input_patterns[3]),
                    op_u!(input_patterns[4]),
                    op_u!(input_patterns[5]),
                    op_u!(input_patterns[6]),
                    op_u!(input_patterns[7]),
                ],
                [
                    op_i!(input_patterns[0]),
                    op_i!(input_patterns[1]),
                    op_i!(input_patterns[2]),
                    op_i!(input_patterns[3]),
                    op_i!(input_patterns[4]),
                    op_i!(input_patterns[5]),
                    op_i!(input_patterns[6]),
                    op_i!(input_patterns[7]),
                ],
            ],
        }
    }
}
