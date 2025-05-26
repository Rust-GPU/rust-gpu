#![doc = include_str!("../README.md")]

/// directory with all the `target-specs` jsons for our codegen backend
#[cfg(feature = "dir_path")]
pub const TARGET_SPEC_DIR_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/target-specs");

#[cfg(feature = "include_str")]
mod include_str;
#[cfg(feature = "include_str")]
pub use include_str::TARGET_SPECS;
