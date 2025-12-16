use std::fmt::{Debug, Formatter};
use thiserror::Error;

pub const SPIRV_TARGET_PREFIX: &str = "spirv-unknown-";

/// A well-formed rust-gpu target.
///
/// The constructors only check whether the target is well-formed, not whether it is valid. Since `spirv-builder` is
/// backwards compatible with older rust-gpu compilers, only the compiler itself knows what targets it can and cannot
/// support. This also allows adding new targets to the compiler without having to update `spirv-builder` and
/// `cargo-gpu`.
///
/// Differentiates between a full target (e.g. `spirv-unknown-vulkan1.3`) and a target env (e.g. `vulkan1.3`). Use
/// [`Self::parse_target`] and [`Self::target`] to parse or format a full target, or use [`Self::parse_env`] and
/// [`Self::env`] when dealing with just target envs. The convenience function [`Self::parse`] accepts both targets and
/// target envs. Does not implement `Display`, since it is unclear whether the user wants a target or target env, though
/// a `Debug` implementation is provided.
#[derive(Clone)]
pub struct SpirvTarget {
    target: String,
}

impl SpirvTarget {
    /// Try to parse either a full target or a target env
    pub fn parse(target_or_env: &str) -> Result<Self, TargetError> {
        Self::parse_target(target_or_env).or_else(|_| Self::parse_env(target_or_env))
    }

    /// Parse a full target, e.g. `spirv-unknown-vulkan1.3`
    pub fn parse_target(target: &str) -> Result<Self, TargetError> {
        let _target_env = target.strip_prefix(SPIRV_TARGET_PREFIX).ok_or_else(|| {
            TargetError::NonSpirvTarget {
                target: target.to_string(),
            }
        })?;
        Ok(Self {
            target: target.to_string(),
        })
    }

    /// Parse a target env, e.g. `vulkan1.3`
    pub fn parse_env(target_env: &str) -> Result<Self, TargetError> {
        Ok(Self {
            target: format!("{SPIRV_TARGET_PREFIX}{target_env}"),
        })
    }

    /// returns the full target, e.g. `spirv-unknown-vulkan1.3`
    pub fn target(&self) -> &str {
        &self.target
    }

    /// returns the target env, e.g. `vulkan1.3`
    pub fn env(&self) -> &str {
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
