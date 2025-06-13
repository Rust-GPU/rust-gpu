#![doc = include_str!("../README.md")]

use strum::{Display, EnumIter, EnumString, IntoStaticStr};

/// directory with all the `target-specs` jsons for our codegen backend
#[cfg(feature = "dir_path")]
pub const TARGET_SPEC_DIR_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/target-specs");

#[cfg(feature = "include_str")]
mod include_str;

pub const SPIRV_ARCH: &str = "spirv";
pub const SPIRV_VENDOR: &str = "unknown";

/// All target envs rust-gpu supports
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
}

impl SpirvTargetEnv {
    pub fn parse_triple(target: &str) -> Option<Self> {
        target
            .strip_prefix(&format!("{SPIRV_ARCH}-{SPIRV_VENDOR}-"))
            .and_then(|env| TryFrom::try_from(env).ok())
    }

    pub fn as_str(&self) -> &'static str {
        self.into()
    }

    pub fn target_triple(&self) -> String {
        format!("{SPIRV_ARCH}-{SPIRV_VENDOR}-{}", self.as_str())
    }

    pub fn target_json_file_name(&self) -> String {
        format!("{SPIRV_ARCH}-{SPIRV_VENDOR}-{}.json", self.as_str())
    }

    #[cfg(feature = "dir_path")]
    pub fn target_json_path(&self) -> String {
        format!(
            "{TARGET_SPEC_DIR_PATH}/{SPIRV_ARCH}-{SPIRV_VENDOR}-{}.json",
            self.as_str()
        )
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
        <Self as strum::IntoEnumIterator>::iter()
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
            let parsed = SpirvTargetEnv::parse_triple(target.as_str()).unwrap();
            assert_eq!(target, parsed);
        }
    }

    #[test]
    #[cfg(feature = "dir_path")]
    pub fn test_target_json_path() {
        for target in SpirvTargetEnv::iter() {
            let file = std::path::PathBuf::from(target.target_json_path());
            assert!(file.is_file());
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
