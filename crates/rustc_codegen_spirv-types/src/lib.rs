#![doc = include_str!("../README.md")]

pub use rspirv::spirv::Capability;

mod compile_result;
pub use compile_result::*;

// HACK(eddyb) allows downstream crates to access the correct version directly.
pub use serde;
pub use serde_json;

/// directory with all the `target-specs` jsons for our codegen backend
pub const TARGET_SPEC_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/target-specs");
