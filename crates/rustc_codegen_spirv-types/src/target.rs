use std::fmt::{Debug, Formatter};
use thiserror::Error;

pub const SPIRV_TARGET_PREFIX: &str = "spirv-unknown-";

/// A well-known rust-gpu target.
///
/// Even if [`Self::from_target`] or [`Self::from_env`] accepts the target, it can still fail to compile if the
/// specific rust-gpu compiler in use offers no support for it.
///
/// While the current implementation is just a validated string, that may change in the future.
#[derive(Clone)]
pub struct SpirvTarget {
    target: String,
}

impl SpirvTarget {
    pub fn from_target(target: &str) -> Result<Self, TargetError> {
        let target_env = target.strip_prefix(SPIRV_TARGET_PREFIX).ok_or_else(|| {
            TargetError::NonSpirvTarget {
                target: target.to_string(),
            }
        })?;

        // used only to split the full list into groups.
        #[allow(clippy::match_same_arms)]
        match target_env {
            // HACK(firestar99) this hardcoded list should be replaced with patterns (eg. `vulkan{}.{}`),
            // which would allow us to support new target versions without updating this list.
            "spv1.0" | "spv1.1" | "spv1.2" | "spv1.3" | "spv1.4" | "spv1.5" | "spv1.6" => {}
            "opengl4.0" | "opengl4.1" | "opengl4.2" | "opengl4.3" | "opengl4.5" => {}
            "vulkan1.0" | "vulkan1.1" | "vulkan1.1spv1.4" | "vulkan1.2" | "vulkan1.3"
            | "vulkan1.4" => {}

            _ => {
                return Err(TargetError::UnsupportedSpirvTargetEnv {
                    target_env: target_env.into(),
                });
            }
        }

        Ok(Self {
            target: target.to_string(),
        })
    }

    pub fn from_env(target_env: &str) -> Result<Self, TargetError> {
        Self::from_target(&format!("{SPIRV_TARGET_PREFIX}{target_env}"))
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
    #[error("SPIR-V target `{SPIRV_TARGET_PREFIX}-{target_env}` is not supported")]
    UnsupportedSpirvTargetEnv { target_env: String },
}
