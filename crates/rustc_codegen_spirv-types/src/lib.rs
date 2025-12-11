#![doc = include_str!("../README.md")]

pub use rspirv::spirv::Capability;

mod compile_result;
mod rustc_version;
mod target_spec;
pub use compile_result::*;
pub use rustc_version::*;
pub use target_spec::*;

// HACK(eddyb) allows downstream crates to access the correct version directly.
pub use serde;
pub use serde_json;
