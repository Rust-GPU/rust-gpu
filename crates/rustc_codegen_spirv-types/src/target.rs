use std::fmt::{Debug, Formatter};
use thiserror::Error;

pub const SPIRV_TARGET_PREFIX: &str = "spirv-unknown-";

/// A well-formed rust-gpu target.
///
/// The constructors [`Self::from_target`] or [`Self::from_env`] only check whether the target is well-formed, not
/// whether it is valid. Since `spirv-builder` is backwards compatible with older rust-gpu compilers, only the compiler
/// itself knows what targets it can and cannot support. This also allows adding new targets to the compiler without
/// having to update `spirv-builder` and `cargo-gpu`.
#[derive(Clone)]
pub struct SpirvTarget {
    target: String,
}

impl SpirvTarget {
    pub fn from_target(target: &str) -> Result<Self, TargetError> {
        let _target_env = target.strip_prefix(SPIRV_TARGET_PREFIX).ok_or_else(|| {
            TargetError::NonSpirvTarget {
                target: target.to_string(),
            }
        })?;
        Ok(Self {
            target: target.to_string(),
        })
    }

    pub fn from_env(target_env: &str) -> Result<Self, TargetError> {
        Ok(Self {
            target: format!("{SPIRV_TARGET_PREFIX}{target_env}"),
        })
    }

    pub fn target(&self) -> &str {
        &self.target
    }

    pub fn target_env(&self) -> &str {
        &self.target[SPIRV_TARGET_PREFIX.len()..]
    }
}

impl Debug for SpirvTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Target").field(&self.target).finish()
    }
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum TargetError {
    #[error("SPIR-V target must start with `{SPIRV_TARGET_PREFIX}...`, was `{target}`")]
    NonSpirvTarget { target: String },
}
