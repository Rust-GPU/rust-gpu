use rspirv::spirv::MemoryModel;
use rustc_codegen_spirv_target_specs::{SPIRV_ARCH, SPIRV_VENDOR, SpirvTargetEnv};
use rustc_target::spec::{Cc, LinkerFlavor, PanicStrategy, Target, TargetOptions};

pub trait TargetsExt {
    fn memory_model(&self) -> MemoryModel;
    fn to_spirv_tools(&self) -> spirv_tools::TargetEnv;
    fn spirv_version(&self) -> (u8, u8);
    fn rustc_target(&self) -> Target;
}

impl TargetsExt for SpirvTargetEnv {
    #[allow(clippy::match_same_arms)]
    fn memory_model(&self) -> MemoryModel {
        match self {
            SpirvTargetEnv::Spv_1_0
            | SpirvTargetEnv::Spv_1_1
            | SpirvTargetEnv::Spv_1_2
            | SpirvTargetEnv::Spv_1_3
            | SpirvTargetEnv::Spv_1_4
            | SpirvTargetEnv::Spv_1_5
            | SpirvTargetEnv::Spv_1_6 => MemoryModel::Simple,

            SpirvTargetEnv::OpenGL_4_0
            | SpirvTargetEnv::OpenGL_4_1
            | SpirvTargetEnv::OpenGL_4_2
            | SpirvTargetEnv::OpenGL_4_3
            | SpirvTargetEnv::OpenGL_4_5 => MemoryModel::GLSL450,

            SpirvTargetEnv::Vulkan_1_0
            | SpirvTargetEnv::Vulkan_1_1
            | SpirvTargetEnv::Vulkan_1_1_Spv_1_4
            | SpirvTargetEnv::Vulkan_1_2
            | SpirvTargetEnv::Vulkan_1_3
            | SpirvTargetEnv::Vulkan_1_4 => MemoryModel::Vulkan,
        }
    }

    #[allow(clippy::match_same_arms)]
    fn to_spirv_tools(&self) -> spirv_tools::TargetEnv {
        match self {
            SpirvTargetEnv::OpenGL_4_0 => spirv_tools::TargetEnv::OpenGL_4_0,
            SpirvTargetEnv::OpenGL_4_1 => spirv_tools::TargetEnv::OpenGL_4_1,
            SpirvTargetEnv::OpenGL_4_2 => spirv_tools::TargetEnv::OpenGL_4_2,
            SpirvTargetEnv::OpenGL_4_3 => spirv_tools::TargetEnv::OpenGL_4_3,
            SpirvTargetEnv::OpenGL_4_5 => spirv_tools::TargetEnv::OpenGL_4_5,
            SpirvTargetEnv::Spv_1_0 => spirv_tools::TargetEnv::Universal_1_0,
            SpirvTargetEnv::Spv_1_1 => spirv_tools::TargetEnv::Universal_1_1,
            SpirvTargetEnv::Spv_1_2 => spirv_tools::TargetEnv::Universal_1_2,
            SpirvTargetEnv::Spv_1_3 => spirv_tools::TargetEnv::Universal_1_3,
            SpirvTargetEnv::Spv_1_4 => spirv_tools::TargetEnv::Universal_1_4,
            SpirvTargetEnv::Spv_1_5 => spirv_tools::TargetEnv::Universal_1_5,
            SpirvTargetEnv::Spv_1_6 => spirv_tools::TargetEnv::Universal_1_6,
            SpirvTargetEnv::Vulkan_1_0 => spirv_tools::TargetEnv::Vulkan_1_0,
            SpirvTargetEnv::Vulkan_1_1 => spirv_tools::TargetEnv::Vulkan_1_1,
            SpirvTargetEnv::Vulkan_1_1_Spv_1_4 => spirv_tools::TargetEnv::Vulkan_1_1_Spirv_1_4,
            SpirvTargetEnv::Vulkan_1_2 => spirv_tools::TargetEnv::Vulkan_1_2,
            SpirvTargetEnv::Vulkan_1_3 => spirv_tools::TargetEnv::Vulkan_1_3,
            SpirvTargetEnv::Vulkan_1_4 => spirv_tools::TargetEnv::Vulkan_1_4,
        }
    }

    fn spirv_version(&self) -> (u8, u8) {
        self.to_spirv_tools().spirv_version()
    }

    fn rustc_target(&self) -> Target {
        let mut o = TargetOptions::default();
        o.simd_types_indirect = false;
        o.allows_weak_linkage = false;
        o.crt_static_allows_dylibs = true;
        o.crt_static_respected = true;
        o.dll_prefix = "".into();
        o.dll_suffix = ".spv.json".into();
        o.dynamic_linking = true;
        o.emit_debug_gdb_scripts = false;
        o.linker_flavor = LinkerFlavor::Unix(Cc::No);
        o.panic_strategy = PanicStrategy::Abort;
        o.env = self.as_str().into();
        o.vendor = SPIRV_VENDOR.into();
        // TODO: Investigate if main_needs_argc_argv is useful (for building exes)
        o.main_needs_argc_argv = false;

        Target {
            llvm_target: self.target_triple().into(),
            metadata: Default::default(),
            pointer_width: 32,
            data_layout: "e-m:e-p:32:32:32-i64:64-n8:16:32:64".into(),
            arch: SPIRV_ARCH.into(),
            options: o,
        }
    }
}
