#![doc = include_str!("../README.md")]

pub use rspirv::spirv::Capability;

mod compile_result;
pub use compile_result::*;

pub mod target_specs;
pub use target_specs::TARGET_SPECS;

// HACK(eddyb) allows downstream crates to access the correct version directly.
pub use serde;
pub use serde_json;
