#![doc = include_str!("../README.md")]

use core::str::FromStr;
use std::fmt::{Debug, Display, Formatter};
use strum::{Display, EnumIter, EnumString, IntoStaticStr};
use thiserror::Error;

/// directory with all the `target-specs` jsons for our codegen backend
#[cfg(feature = "dir_path")]
pub const TARGET_SPEC_DIR_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/target-specs");

#[cfg(feature = "include_str")]
mod include_str;
#[cfg(feature = "serde")]
mod serde_feature;
#[cfg(feature = "serde")]
pub use serde_feature::*;

pub const SPIRV_ARCH: &str = "spirv";
pub const SPIRV_VENDOR: &str = "unknown";
pub const SPIRV_TARGET_PREFIX: &str = "spirv-unknown-";

/// All target envs rust-gpu supports. The corresponding target tripple is `spirv-unknown-ENV` with `ENV` replaced by any of the values below.
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, EnumString, IntoStaticStr, EnumIter, Display)]
pub enum SpirvTargetEnv {
    #[strum(to_string = "opengl4.0")]
    OpenGL_4_0,
    #[strum(to_string = "opengl4.1")]
    OpenGL_4_1,
    #[strum(to_string = "opengl4.2")]
    OpenGL_4_2,
    #[strum(to_string = "opengl4.3")]
    OpenGL_4_3,
    #[strum(to_string = "opengl4.5")]
    OpenGL_4_5,
    #[strum(to_string = "spv1.0")]
    Spv_1_0,
    #[strum(to_string = "spv1.1")]
    Spv_1_1,
    #[strum(to_string = "spv1.2")]
    Spv_1_2,
    #[strum(to_string = "spv1.3")]
    Spv_1_3,
    #[strum(to_string = "spv1.4")]
    Spv_1_4,
    #[strum(to_string = "spv1.5")]
    Spv_1_5,
    #[strum(to_string = "spv1.6")]
    Spv_1_6,
    #[strum(to_string = "vulkan1.0")]
    Vulkan_1_0,
    #[strum(to_string = "vulkan1.1")]
    Vulkan_1_1,
    #[strum(to_string = "vulkan1.1spv1.4")]
    Vulkan_1_1_Spv_1_4,
    #[strum(to_string = "vulkan1.2")]
    Vulkan_1_2,
    #[strum(to_string = "vulkan1.3")]
    Vulkan_1_3,
    #[strum(to_string = "vulkan1.4")]
    Vulkan_1_4,
    #[strum(to_string = "naga-wgsl")]
    Naga_Wgsl,
}

#[derive(Clone, Error, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum SpirvTargetParseError {
    #[error("Expected `rustc_codegen_spirv` target with prefix `{SPIRV_TARGET_PREFIX}`, got `{0}`")]
    WrongPrefix(String),
    #[error(
        "Target `{SPIRV_TARGET_PREFIX}{0}` not supported by `rustc_codegen_spirv`, see `enum SpirvTargetEnv` for possible env values"
    )]
    UnknownEnv(String),
}

impl Debug for SpirvTargetParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl SpirvTargetEnv {
    pub fn parse_triple(target: &str) -> Result<Self, SpirvTargetParseError> {
        let env = target
            .strip_prefix(SPIRV_TARGET_PREFIX)
            .ok_or_else(|| SpirvTargetParseError::WrongPrefix(target.to_string()))?;
        FromStr::from_str(env).map_err(|_e| SpirvTargetParseError::UnknownEnv(env.to_string()))
    }

    pub fn as_str(&self) -> &'static str {
        self.into()
    }

    pub fn target_triple(&self) -> String {
        format!("{SPIRV_TARGET_PREFIX}{}", self.as_str())
    }

    pub fn target_json_file_name(&self) -> String {
        format!("{SPIRV_TARGET_PREFIX}{}.json", self.as_str())
    }

    #[cfg(feature = "dir_path")]
    pub fn target_json_path(&self) -> String {
        format!(
            "{TARGET_SPEC_DIR_PATH}/{SPIRV_TARGET_PREFIX}{}.json",
            self.as_str()
        )
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
        <Self as strum::IntoEnumIterator>::iter()
    }
}

pub trait IntoSpirvTarget: Sized {
    fn to_spirv_target_env(&self) -> Result<SpirvTargetEnv, SpirvTargetParseError>;
}

impl IntoSpirvTarget for SpirvTargetEnv {
    fn to_spirv_target_env(&self) -> Result<SpirvTargetEnv, SpirvTargetParseError> {
        Ok(*self)
    }
}

impl IntoSpirvTarget for &str {
    fn to_spirv_target_env(&self) -> Result<SpirvTargetEnv, SpirvTargetParseError> {
        SpirvTargetEnv::parse_triple(self)
    }
}

impl IntoSpirvTarget for String {
    fn to_spirv_target_env(&self) -> Result<SpirvTargetEnv, SpirvTargetParseError> {
        SpirvTargetEnv::parse_triple(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_prefix() {
        assert_eq!(
            SPIRV_TARGET_PREFIX,
            &format!("{SPIRV_ARCH}-{SPIRV_VENDOR}-")
        );
    }

    #[test]
    pub fn test_triple_parse_roundtrip() {
        for target in SpirvTargetEnv::iter() {
            let parsed = SpirvTargetEnv::parse_triple(&target.target_triple()).unwrap();
            assert_eq!(target, parsed);
        }
    }

    #[test]
    #[cfg(feature = "dir_path")]
    pub fn test_target_json_path() {
        for target in SpirvTargetEnv::iter() {
            let file = std::path::PathBuf::from(target.target_json_path());
            assert!(file.is_file(), "{}", file.display());
        }
    }

    #[test]
    #[cfg(all(feature = "dir_path", feature = "include_str"))]
    pub fn test_target_json_content() {
        for target in SpirvTargetEnv::iter() {
            let content = std::fs::read_to_string(target.target_json_path()).unwrap();
            assert_eq!(content, target.include_str());
        }
    }
}
