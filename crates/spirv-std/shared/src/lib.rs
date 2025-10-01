#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]

pub mod image_params;
#[cfg(feature = "std")]
pub mod spirv_attr_version;
