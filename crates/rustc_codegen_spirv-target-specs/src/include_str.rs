//! Metadata for the compile targets supported by `rust-gpu`

use crate::SpirvTargetEnv;

impl SpirvTargetEnv {
    pub fn include_str(&self) -> &'static str {
        match self {
            SpirvTargetEnv::OpenGL_4_0 => {
                include_str!("../target-specs/spirv-unknown-opengl4.0.json")
            }
            SpirvTargetEnv::OpenGL_4_1 => {
                include_str!("../target-specs/spirv-unknown-opengl4.1.json")
            }
            SpirvTargetEnv::OpenGL_4_2 => {
                include_str!("../target-specs/spirv-unknown-opengl4.2.json")
            }
            SpirvTargetEnv::OpenGL_4_3 => {
                include_str!("../target-specs/spirv-unknown-opengl4.3.json")
            }
            SpirvTargetEnv::OpenGL_4_5 => {
                include_str!("../target-specs/spirv-unknown-opengl4.5.json")
            }
            SpirvTargetEnv::Spv_1_0 => {
                include_str!("../target-specs/spirv-unknown-spv1.0.json")
            }
            SpirvTargetEnv::Spv_1_1 => {
                include_str!("../target-specs/spirv-unknown-spv1.1.json")
            }
            SpirvTargetEnv::Spv_1_2 => {
                include_str!("../target-specs/spirv-unknown-spv1.2.json")
            }
            SpirvTargetEnv::Spv_1_3 => {
                include_str!("../target-specs/spirv-unknown-spv1.3.json")
            }
            SpirvTargetEnv::Spv_1_4 => {
                include_str!("../target-specs/spirv-unknown-spv1.4.json")
            }
            SpirvTargetEnv::Spv_1_5 => {
                include_str!("../target-specs/spirv-unknown-spv1.5.json")
            }
            SpirvTargetEnv::Spv_1_6 => {
                include_str!("../target-specs/spirv-unknown-spv1.6.json")
            }
            SpirvTargetEnv::Vulkan_1_0 => {
                include_str!("../target-specs/spirv-unknown-vulkan1.0.json")
            }
            SpirvTargetEnv::Vulkan_1_1 => {
                include_str!("../target-specs/spirv-unknown-vulkan1.1.json")
            }
            SpirvTargetEnv::Vulkan_1_1_Spv_1_4 => {
                include_str!("../target-specs/spirv-unknown-vulkan1.1spv1.4.json")
            }
            SpirvTargetEnv::Vulkan_1_2 => {
                include_str!("../target-specs/spirv-unknown-vulkan1.2.json")
            }
            SpirvTargetEnv::Vulkan_1_3 => {
                include_str!("../target-specs/spirv-unknown-vulkan1.3.json")
            }
            SpirvTargetEnv::Vulkan_1_4 => {
                include_str!("../target-specs/spirv-unknown-vulkan1.4.json")
            }
        }
    }
}
