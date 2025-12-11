// FIXME(eddyb) update/review these lints.
//
// BEGIN - Embark standard lints v0.4
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
#![deny(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::char_lit_as_u8,
    clippy::checked_conversions,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::expl_impl_clone_on_copy,
    clippy::explicit_deref_methods,
    clippy::explicit_into_iter_loop,
    clippy::fallible_impl_from,
    clippy::filter_map_next,
    clippy::float_cmp_const,
    clippy::fn_params_excessive_bools,
    clippy::if_let_mutex,
    clippy::implicit_clone,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::invalid_upcast_comparisons,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::manual_ok_or,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mut_mut,
    clippy::mutex_integer,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::path_buf_push_overwrite,
    clippy::ptr_as_ptr,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_functions_in_if_condition,
    clippy::semicolon_if_nothing_returned,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::todo,
    clippy::trait_duplication_in_bounds,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    clippy::zero_sized_map_values,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
// END - Embark standard lints v0.4
// crate-specific exceptions:
// #![allow()]
#![doc = include_str!("../README.md")]

pub mod cargo_cmd;
mod depfile;
#[cfg(feature = "watch")]
mod watch;

use raw_string::{RawStr, RawString};
use semver::Version;
use serde::Deserialize;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use thiserror::Error;

#[cfg(feature = "watch")]
pub use self::watch::{SpirvWatcher, SpirvWatcherError};
pub use rustc_codegen_spirv_types::Capability;
use rustc_codegen_spirv_types::query_rustc_version;
pub use rustc_codegen_spirv_types::{CompileResult, ModuleResult, TargetSpecVersion};

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum SpirvBuilderError {
    #[error("`target` must be set, for example `spirv-unknown-vulkan1.2`")]
    MissingTarget,
    #[error("expected `{SPIRV_TARGET_PREFIX}...` target, found `{target}`")]
    NonSpirvTarget { target: String },
    #[error("SPIR-V target `{SPIRV_TARGET_PREFIX}-{target_env}` is not supported")]
    UnsupportedSpirvTargetEnv { target_env: String },
    #[error("`path_to_crate` must be set")]
    MissingCratePath,
    #[error("crate path '{0}' does not exist")]
    CratePathDoesntExist(PathBuf),
    #[error(
        "Without feature `rustc_codegen_spirv`, you need to set the path of the dylib with `rustc_codegen_spirv_location`"
    )]
    MissingRustcCodegenSpirvDylib,
    #[error("`rustc_codegen_spirv_location` path '{0}' is not a file")]
    RustcCodegenSpirvDylibDoesNotExist(PathBuf),
    #[error("build failed")]
    BuildFailed,
    #[error("multi-module build cannot be used with print_metadata = MetadataPrintout::Full")]
    MultiModuleWithPrintMetadata,
    #[error("multi-module metadata file missing")]
    MetadataFileMissing(#[from] std::io::Error),
    #[error("unable to parse multi-module metadata file")]
    MetadataFileMalformed(#[from] serde_json::Error),
    #[error(
        "`{ARTIFACT_SUFFIX}` artifact not found in (supposedly successful) build output.\n--- build output ---\n{stdout}"
    )]
    NoArtifactProduced { stdout: String },
    #[error("cargo metadata error")]
    CargoMetadata(#[from] cargo_metadata::Error),
    #[cfg(feature = "watch")]
    #[error(transparent)]
    WatchFailed(#[from] SpirvWatcherError),
}

const SPIRV_TARGET_PREFIX: &str = "spirv-unknown-";

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
#[non_exhaustive]
pub enum MetadataPrintout {
    /// Print no cargo metadata.
    #[default]
    None,
    /// Print only dependency information (eg for multiple modules).
    DependencyOnly,
    /// Print all cargo metadata.
    ///
    /// Includes dependency information and spirv environment variable.
    Full,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
#[non_exhaustive]
pub enum SpirvMetadata {
    /// Strip all names and other debug information from SPIR-V output.
    #[default]
    None,
    /// Only include `OpName`s for public interface variables (uniforms and the like), to allow
    /// shader reflection.
    NameVariables,
    /// Include all `OpName`s for everything, and `OpLine`s. Significantly increases binary size.
    Full,
}

/// Strategy used to handle Rust `panic!`s in shaders compiled to SPIR-V.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
#[non_exhaustive]
pub enum ShaderPanicStrategy {
    /// Return from shader entry-point with no side-effects **(default)**.
    ///
    /// While similar to the standard SPIR-V `OpTerminateInvocation`, this is
    /// *not* limited to fragment shaders, and instead supports all shaders
    /// (as it's handled via control-flow rewriting, instead of SPIR-V features).
    #[default]
    SilentExit,

    /// Like `SilentExit`, but also using `debugPrintf` to report the panic in
    /// a way that can reach the user, before returning from the entry-point.
    ///
    /// Quick setup for enabling `debugPrintf` output (to stdout) at runtime:
    /// - **set these environment variables**:
    ///   - `VK_LOADER_LAYERS_ENABLE=VK_LAYER_KHRONOS_validation`
    ///   - `VK_LAYER_PRINTF_ONLY_PRESET=1`
    ///   - `VK_LAYER_PRINTF_TO_STDOUT=1` (not always needed, but can help)
    /// - if using `wgpu`, enable `wgpu::Features::SPIRV_SHADER_PASSTHROUGH`,
    ///   and use `create_shader_module_passthrough` instead of `create_shader_module`
    /// - in case of errors, or no output (from a `panic!()`/`debug_printf!()`),
    ///   keep reading below for additional information and alternatives
    ///
    /// ---
    ///
    /// **Note**: enabling this automatically adds the `SPV_KHR_non_semantic_info`
    /// extension, as `debugPrintf` is from a "non-semantic extended instruction set".
    ///
    /// **Note**: `debugPrintf` output reaching the user involves:
    /// - being able to load the shader in the first place:
    ///   - for `wgpu`, use "SPIR-V shader passthrough" (Naga lacks `debugPrintf`):
    ///     - enable `wgpu::Features::SPIRV_SHADER_PASSTHROUGH`
    ///     - replace `create_shader_module` calls with `create_shader_module_passthrough`
    ///   - *in theory*, the `VK_KHR_shader_non_semantic_info` Vulkan *Device* extension
    ///     (or requiring at least Vulkan 1.3, which incorporated it)
    ///     - *however*, Validation Layers don't actually check this anymore,
    ///       since Vulkan SDK version 1.4.313.0 (and drivers shouldn't care either)
    /// - **general configurability** of [Vulkan SDK](https://vulkan.lunarg.com/sdk/home)
    ///   and/or [Vulkan Loader](https://github.com/KhronosGroup/Vulkan-Loader)
    ///   - *(this list doubles as a legend for shorthands used later below)*
    ///   - **env**: setting environment variables on the fly
    ///     - easiest for quick testing, no code changes/rebuilding needed
    ///     - e.g. `FOO=1 cargo run ...` (in UNIX-style shells)
    ///   - **instance**: programmatic control via `vkCreateInstance()` params
    ///     - best for integration with app-specific debugging functionality
    ///     - limited to direct Vulkan usage (e.g. `ash`, not `wgpu`)
    ///     - `VK_EXT_layer_settings` as a `VK_LAYER_*` environment variables
    ///       analogue, e.g. `VK_LAYER_FOO` controlled by a `VkLayerSettingEXT`
    ///       with `"foo"` as `pSettingName` (and an appropriate `type`/value),
    ///       included in `VkLayerSettingsCreateInfoEXT`'s `pSettings`
    ///   - on-disk configuration and interactive tooling, e.g.:
    ///     - `vk_layer_settings.txt` files, either hand-written, or generated by
    ///       the "Vulkan Configurator" GUI tool (included with the Vulkan SDK)
    ///     - third-party Vulkan debuggers like `RenderDoc`
    /// - [Vulkan Validation Layers](https://github.com/KhronosGroup/Vulkan-ValidationLayers)
    ///   - (they contain the `debugPrintf` implementation, a SPIR-V -> SPIR-V translation)
    ///   - enabled by one of (as per "**general configurability**" above):
    ///     - **env**: `VK_LOADER_LAYERS_ENABLE=VK_LAYER_KHRONOS_validation`
    ///     - **instance**: `"VK_LAYER_KHRONOS_validation"` in the list of layers
    ///     - via `wgpu`: `wgpu::InstanceFlags::VALIDATION`
    /// - Validation Layers' `debugPrintf` support
    ///   ([official docs](https://github.com/KhronosGroup/Vulkan-ValidationLayers/blob/main/docs/debug_printf.md)):
    ///   - enabled by one of (as per "**general configurability**" above):
    ///     - **env**: `VK_LAYER_PRINTF_ENABLE=1` (validation + `debugPrintf`)
    ///     - **env**: `VK_LAYER_PRINTF_ONLY_PRESET=1` (*only* `debugPrintf`, no validation)
    ///     - **instance**: `"printf_enable"` / `"printf_only_preset"` via `VkLayerSettingEXT`
    ///       (i.e. analogues for the two environment variables)
    ///     - **instance**: `VkValidationFeaturesEXT` with `pEnabledValidationFeatures`
    ///       containing `VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT`
    /// - outputting the `debugPrintf` messages sent back from the GPU:
    ///   - defaults to common validation logging (itself defaulting to stdout)
    ///   - **env**: `VK_LAYER_PRINTF_TO_STDOUT=1` (and its **instance** analogue)
    ///     forces direct printing to stdout, bypassing `VK_EXT_debug_utils` etc.
    ///   - validation logging can itself be controlled via `VK_EXT_debug_utils`
    ///   - `wgpu` built in debug mode (and/or with debug-assertions enabled):
    ///     - it uses `VK_EXT_debug_utils` internally, exposing it via `log`
    ///     - with e.g. `env_logger`, `RUST_LOG=info` suffices for `debugPrintf`
    ///       messages (as they specifically have the "info" level)
    ///     - other `log`/`tracing` subscribers should be configured similarly
    #[cfg_attr(feature = "clap", clap(skip))]
    DebugPrintfThenExit {
        /// Whether to also print the entry-point inputs (excluding buffers/resources),
        /// which should uniquely identify the panicking shader invocation.
        print_inputs: bool,

        /// Whether to also print a "backtrace" (i.e. the chain of function calls
        /// that led to the `panic!`).
        ///
        /// As there is no way to dynamically compute this information, the string
        /// containing the full backtrace of each `panic!` is statically generated,
        /// meaning this option could significantly increase binary size.
        print_backtrace: bool,
    },

    /// **Warning**: this is _**unsound**_ (i.e. adds Undefined Behavior to *safe* Rust code)
    ///
    /// This option only exists for testing (hence the unfriendly name it has),
    /// and more specifically testing whether conditional panics are responsible
    /// for performance differences when upgrading from older Rust-GPU versions
    /// (which used infinite loops for panics, that `spirv-opt`/drivers could've
    /// sometimes treated as UB, and optimized as if they were impossible to reach).
    ///
    /// Unlike those infinite loops, however, this uses `OpUnreachable`, so it
    /// forces the old worst-case (all `panic!`s become UB and are optimized out).
    #[allow(non_camel_case_types)]
    UNSOUND_DO_NOT_USE_UndefinedBehaviorViaUnreachable,
}

/// Options for specifying the behavior of the validator
/// Copied from `spirv-tools/src/val.rs` struct `ValidatorOptions`, with some fields disabled.
#[derive(Default, Debug, Clone, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[non_exhaustive]
pub struct ValidatorOptions {
    /// Record whether or not the validator should relax the rules on types for
    /// stores to structs.  When relaxed, it will allow a type mismatch as long as
    /// the types are structs with the same layout.  Two structs have the same layout
    /// if
    ///
    /// 1) the members of the structs are either the same type or are structs with
    ///    same layout, and
    ///
    /// 2) the decorations that affect the memory layout are identical for both
    ///    types.  Other decorations are not relevant.
    #[cfg_attr(feature = "clap", arg(long, default_value = "false"))]
    pub relax_struct_store: bool,
    /// Records whether or not the validator should relax the rules on pointer usage
    /// in logical addressing mode.
    ///
    /// When relaxed, it will allow the following usage cases of pointers:
    /// 1) `OpVariable` allocating an object whose type is a pointer type
    /// 2) `OpReturnValue` returning a pointer value
    #[cfg_attr(feature = "clap", arg(long, default_value = "false"))]
    pub relax_logical_pointer: bool,
    // /// Records whether or not the validator should relax the rules because it is
    // /// expected that the optimizations will make the code legal.
    // ///
    // /// When relaxed, it will allow the following:
    // /// 1) It will allow relaxed logical pointers.  Setting this option will also
    // ///    set that option.
    // /// 2) Pointers that are pass as parameters to function calls do not have to
    // ///    match the storage class of the formal parameter.
    // /// 3) Pointers that are actaul parameters on function calls do not have to point
    // ///    to the same type pointed as the formal parameter.  The types just need to
    // ///    logically match.
    // pub before_legalization: bool,
    /// Records whether the validator should use "relaxed" block layout rules.
    /// Relaxed layout rules are described by Vulkan extension
    /// `VK_KHR_relaxed_block_layout`, and they affect uniform blocks, storage blocks,
    /// and push constants.
    ///
    /// This is enabled by default when targeting Vulkan 1.1 or later.
    /// Relaxed layout is more permissive than the default rules in Vulkan 1.0.
    #[cfg_attr(feature = "clap", arg(long, default_value = "false"))]
    pub relax_block_layout: Option<bool>,
    /// Records whether the validator should use standard block layout rules for
    /// uniform blocks.
    #[cfg_attr(feature = "clap", arg(long, default_value = "false"))]
    pub uniform_buffer_standard_layout: bool,
    /// Records whether the validator should use "scalar" block layout rules.
    /// Scalar layout rules are more permissive than relaxed block layout.
    ///
    /// See Vulkan extnesion `VK_EXT_scalar_block_layout`.  The scalar alignment is
    /// defined as follows:
    /// - scalar alignment of a scalar is the scalar size
    /// - scalar alignment of a vector is the scalar alignment of its component
    /// - scalar alignment of a matrix is the scalar alignment of its component
    /// - scalar alignment of an array is the scalar alignment of its element
    /// - scalar alignment of a struct is the max scalar alignment among its
    ///   members
    ///
    /// For a struct in Uniform, `StorageClass`, or `PushConstant`:
    /// - a member Offset must be a multiple of the member's scalar alignment
    /// - `ArrayStride` or `MatrixStride` must be a multiple of the array or matrix
    ///   scalar alignment
    #[cfg_attr(feature = "clap", arg(long, default_value = "false"))]
    pub scalar_block_layout: bool,
    /// Records whether or not the validator should skip validating standard
    /// uniform/storage block layout.
    #[cfg_attr(feature = "clap", arg(long, default_value = "false"))]
    pub skip_block_layout: bool,
    // /// Applies a maximum to one or more Universal limits
    // pub max_limits: Vec<(ValidatorLimits, u32)>,
}

/// Options for specifying the behavior of the optimizer
/// Copied from `spirv-tools/src/opt.rs` struct `Options`, with some fields disabled.
#[derive(Default, Debug, Clone, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[non_exhaustive]
pub struct OptimizerOptions {
    // /// Records the validator options that should be passed to the validator,
    // /// the validator will run with the options before optimizer.
    // pub validator_options: Option<crate::val::ValidatorOptions>,
    // /// Records the maximum possible value for the id bound.
    // pub max_id_bound: Option<u32>,
    /// Records whether all bindings within the module should be preserved.
    #[cfg_attr(feature = "clap", arg(long, default_value = "false"))]
    pub preserve_bindings: bool,
    // /// Records whether all specialization constants within the module
    // /// should be preserved.
    // pub preserve_spec_constants: bool,
}

/// Cargo features specification for building the shader crate.
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[non_exhaustive]
pub struct ShaderCrateFeatures {
    /// Set --default-features for the target shader crate.
    #[cfg_attr(feature = "clap", clap(long = "no-default-features", default_value = "true", action = clap::ArgAction::SetFalse))]
    pub default_features: bool,
    /// Set --features for the target shader crate.
    #[cfg_attr(feature = "clap", clap(long))]
    pub features: Vec<String>,
}

impl Default for ShaderCrateFeatures {
    fn default() -> Self {
        Self {
            default_features: true,
            features: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[non_exhaustive]
pub struct SpirvBuilder {
    /// The path to the shader crate to compile
    #[cfg_attr(feature = "clap", clap(skip))]
    pub path_to_crate: Option<PathBuf>,
    /// The cargo command to run, formatted like `cargo {cargo_cmd} ...`. Defaults to `rustc`.
    #[cfg_attr(feature = "clap", clap(skip))]
    pub cargo_cmd: Option<String>,
    /// Whether the cargo command set in `cargo_cmd` behaves like `cargo rustc` and allows passing args such as
    /// `--crate-type dylib`. Defaults to true if `cargo_cmd` is `None` or `Some("rustc")`.
    #[cfg_attr(feature = "clap", clap(skip))]
    pub cargo_cmd_like_rustc: Option<bool>,
    /// Whether to print build.rs cargo metadata (e.g. cargo:rustc-env=var=val). Defaults to [`MetadataPrintout::None`].
    /// Within build scripts, set it to [`MetadataPrintout::DependencyOnly`] or [`MetadataPrintout::Full`] to ensure the build script is rerun on code changes.
    #[cfg_attr(feature = "clap", clap(skip))]
    pub print_metadata: MetadataPrintout,
    /// Build in release. Defaults to true.
    #[cfg_attr(feature = "clap", clap(long = "debug", default_value = "true", action = clap::ArgAction::SetFalse))]
    pub release: bool,
    /// The target triple, eg. `spirv-unknown-vulkan1.2`
    #[cfg_attr(
        feature = "clap",
        clap(long, default_value = "spirv-unknown-vulkan1.2")
    )]
    pub target: Option<String>,
    /// Cargo features specification for building the shader crate.
    #[cfg_attr(feature = "clap", clap(flatten))]
    #[serde(flatten)]
    pub shader_crate_features: ShaderCrateFeatures,
    /// Deny any warnings, as they may never be printed when building within a build script. Defaults to false.
    #[cfg_attr(feature = "clap", arg(long, default_value = "false"))]
    pub deny_warnings: bool,
    /// Splits the resulting SPIR-V file into one module per entry point. This is useful in cases
    /// where ecosystem tooling has bugs around multiple entry points per module - having all entry
    /// points bundled into a single file is the preferred system.
    #[cfg_attr(feature = "clap", arg(long, default_value = "false"))]
    pub multimodule: bool,
    /// Sets the level of metadata (primarily `OpName` and `OpLine`) included in the SPIR-V binary.
    /// Including metadata significantly increases binary size.
    #[cfg_attr(feature = "clap", arg(long, default_value = "none"))]
    pub spirv_metadata: SpirvMetadata,
    /// Adds a capability to the SPIR-V module. Checking if a capability is enabled in code can be
    /// done via `#[cfg(target_feature = "TheCapability")]`.
    #[cfg_attr(feature = "clap", arg(long, value_parser=Self::parse_spirv_capability))]
    pub capabilities: Vec<Capability>,
    /// Adds an extension to the SPIR-V module. Checking if an extension is enabled in code can be
    /// done via `#[cfg(target_feature = "ext:the_extension")]`.
    #[cfg_attr(feature = "clap", arg(long))]
    pub extensions: Vec<String>,
    /// Set additional "codegen arg". Note: the `RUSTGPU_CODEGEN_ARGS` environment variable
    /// takes precedence over any set arguments using this function.
    #[cfg_attr(feature = "clap", clap(skip))]
    pub extra_args: Vec<String>,
    // Location of a known `rustc_codegen_spirv` dylib, only required without feature `rustc_codegen_spirv`.
    #[cfg_attr(feature = "clap", clap(skip))]
    pub rustc_codegen_spirv_location: Option<PathBuf>,
    // Overwrite the toolchain like `cargo +nightly`
    #[cfg_attr(feature = "clap", clap(skip))]
    pub toolchain_overwrite: Option<String>,
    // Set the rustc version of the toolchain, used to adjust params to support older toolchains
    #[cfg_attr(feature = "clap", clap(skip))]
    pub toolchain_rustc_version: Option<Version>,

    /// Set the target dir path to use for building shaders. Relative paths will be resolved
    /// relative to the `target` dir of the shader crate, absolute paths are used as is.
    /// Defaults to `spirv-builder`, resulting in the path `./target/spirv-builder`.
    #[cfg_attr(feature = "clap", clap(skip))]
    pub target_dir_path: Option<PathBuf>,

    // `rustc_codegen_spirv::linker` codegen args
    /// Change the shader `panic!` handling strategy (see [`ShaderPanicStrategy`]).
    #[cfg_attr(feature = "clap", clap(skip))]
    pub shader_panic_strategy: ShaderPanicStrategy,

    /// spirv-val flags
    #[cfg_attr(feature = "clap", clap(flatten))]
    #[serde(flatten)]
    pub validator: ValidatorOptions,

    /// spirv-opt flags
    #[cfg_attr(feature = "clap", clap(flatten))]
    #[serde(flatten)]
    pub optimizer: OptimizerOptions,
}

#[cfg(feature = "clap")]
impl SpirvBuilder {
    /// Clap value parser for `Capability`.
    fn parse_spirv_capability(capability: &str) -> Result<Capability, clap::Error> {
        use core::str::FromStr;
        Capability::from_str(capability).map_or_else(
            |()| Err(clap::Error::new(clap::error::ErrorKind::InvalidValue)),
            Ok,
        )
    }
}

impl Default for SpirvBuilder {
    fn default() -> Self {
        Self {
            path_to_crate: None,
            cargo_cmd: None,
            cargo_cmd_like_rustc: None,
            print_metadata: MetadataPrintout::default(),
            release: true,
            target: None,
            deny_warnings: false,
            multimodule: false,
            spirv_metadata: SpirvMetadata::default(),
            capabilities: Vec::new(),
            extensions: Vec::new(),
            extra_args: Vec::new(),
            rustc_codegen_spirv_location: None,
            target_dir_path: None,
            toolchain_overwrite: None,
            toolchain_rustc_version: None,
            shader_panic_strategy: ShaderPanicStrategy::default(),
            validator: ValidatorOptions::default(),
            optimizer: OptimizerOptions::default(),
            shader_crate_features: ShaderCrateFeatures::default(),
        }
    }
}

impl SpirvBuilder {
    pub fn new(path_to_crate: impl AsRef<Path>, target: impl Into<String>) -> Self {
        Self {
            path_to_crate: Some(path_to_crate.as_ref().to_owned()),
            target: Some(target.into()),
            ..SpirvBuilder::default()
        }
    }

    /// Whether to print build.rs cargo metadata (e.g. cargo:rustc-env=var=val). Defaults to [`MetadataPrintout::Full`].
    #[must_use]
    pub fn print_metadata(mut self, v: MetadataPrintout) -> Self {
        self.print_metadata = v;
        self
    }

    #[must_use]
    pub fn deny_warnings(mut self, v: bool) -> Self {
        self.deny_warnings = v;
        self
    }

    /// Build in release. Defaults to true.
    #[must_use]
    pub fn release(mut self, v: bool) -> Self {
        self.release = v;
        self
    }

    /// Splits the resulting SPIR-V file into one module per entry point. This is useful in cases
    /// where ecosystem tooling has bugs around multiple entry points per module - having all entry
    /// points bundled into a single file is the preferred system.
    #[must_use]
    pub fn multimodule(mut self, v: bool) -> Self {
        self.multimodule = v;
        self
    }

    /// Sets the level of metadata (primarily `OpName` and `OpLine`) included in the SPIR-V binary.
    /// Including metadata significantly increases binary size.
    #[must_use]
    pub fn spirv_metadata(mut self, v: SpirvMetadata) -> Self {
        self.spirv_metadata = v;
        self
    }

    /// Adds a capability to the SPIR-V module. Checking if a capability is enabled in code can be
    /// done via `#[cfg(target_feature = "TheCapability")]`.
    #[must_use]
    pub fn capability(mut self, capability: Capability) -> Self {
        self.capabilities.push(capability);
        self
    }

    /// Adds an extension to the SPIR-V module. Checking if an extension is enabled in code can be
    /// done via `#[cfg(target_feature = "ext:the_extension")]`.
    #[must_use]
    pub fn extension(mut self, extension: impl Into<String>) -> Self {
        self.extensions.push(extension.into());
        self
    }

    /// Change the shader `panic!` handling strategy (see [`ShaderPanicStrategy`]).
    #[must_use]
    pub fn shader_panic_strategy(mut self, shader_panic_strategy: ShaderPanicStrategy) -> Self {
        self.shader_panic_strategy = shader_panic_strategy;
        self
    }

    /// Allow store from one struct type to a different type with compatible layout and members.
    #[must_use]
    pub fn relax_struct_store(mut self, v: bool) -> Self {
        self.validator.relax_struct_store = v;
        self
    }

    /// Allow allocating an object of a pointer type and returning a pointer value from a function
    /// in logical addressing mode
    #[must_use]
    pub fn relax_logical_pointer(mut self, v: bool) -> Self {
        self.validator.relax_logical_pointer = v;
        self
    }

    /// Enable `VK_KHR_relaxed_block_layout` when checking standard uniform, storage buffer, and
    /// push constant layouts. This is the default when targeting Vulkan 1.1 or later.
    #[must_use]
    pub fn relax_block_layout(mut self, v: bool) -> Self {
        self.validator.relax_block_layout = Some(v);
        self
    }

    /// Enable `VK_KHR_uniform_buffer_standard_layout` when checking standard uniform buffer
    /// layouts.
    #[must_use]
    pub fn uniform_buffer_standard_layout(mut self, v: bool) -> Self {
        self.validator.uniform_buffer_standard_layout = v;
        self
    }

    /// Enable `VK_EXT_scalar_block_layout` when checking standard uniform, storage buffer, and
    /// push constant layouts. Scalar layout rules are more permissive than relaxed block layout so
    /// in effect this will override the --relax-block-layout option.
    #[must_use]
    pub fn scalar_block_layout(mut self, v: bool) -> Self {
        self.validator.scalar_block_layout = v;
        self
    }

    /// Skip checking standard uniform/storage buffer layout. Overrides any --relax-block-layout or
    /// --scalar-block-layout option.
    #[must_use]
    pub fn skip_block_layout(mut self, v: bool) -> Self {
        self.validator.skip_block_layout = v;
        self
    }

    /// Preserve unused descriptor bindings. Useful for reflection.
    #[must_use]
    pub fn preserve_bindings(mut self, v: bool) -> Self {
        self.optimizer.preserve_bindings = v;
        self
    }

    /// Set additional "codegen arg". Note: the `RUSTGPU_CODEGEN_ARGS` environment variable
    /// takes precedence over any set arguments using this function.
    #[must_use]
    pub fn extra_arg(mut self, arg: impl Into<String>) -> Self {
        self.extra_args.push(arg.into());
        self
    }

    /// Set --default-features for the target shader crate.
    #[must_use]
    pub fn shader_crate_default_features(mut self, default_features: bool) -> Self {
        self.shader_crate_features.default_features = default_features;
        self
    }

    /// Set --features for the target shader crate.
    #[must_use]
    pub fn shader_crate_features(mut self, features: impl IntoIterator<Item = String>) -> Self {
        self.shader_crate_features.features = features.into_iter().collect();
        self
    }

    #[must_use]
    pub fn rustc_codegen_spirv_location(mut self, path_to_dylib: impl AsRef<Path>) -> Self {
        self.rustc_codegen_spirv_location = Some(path_to_dylib.as_ref().to_path_buf());
        self
    }

    /// Set the target dir path to use for building shaders. Relative paths will be resolved
    /// relative to the `target` dir of the shader crate, absolute paths are used as is.
    /// Defaults to `spirv-builder`, resulting in the path `./target/spirv-builder`.
    #[must_use]
    pub fn target_dir_path(mut self, name: impl Into<PathBuf>) -> Self {
        self.target_dir_path = Some(name.into());
        self
    }

    /// Builds the module. If `print_metadata` is [`MetadataPrintout::Full`], you usually don't have to inspect the path
    /// in the result, as the environment variable for the path to the module will already be set.
    pub fn build(&self) -> Result<CompileResult, SpirvBuilderError> {
        let metadata_file = invoke_rustc(self)?;
        match self.print_metadata {
            MetadataPrintout::Full | MetadataPrintout::DependencyOnly => {
                leaf_deps(&metadata_file, |artifact| {
                    println!("cargo:rerun-if-changed={artifact}");
                })
                // Close enough
                .map_err(SpirvBuilderError::MetadataFileMissing)?;
            }
            MetadataPrintout::None => (),
        }
        let metadata = self.parse_metadata_file(&metadata_file)?;

        Ok(metadata)
    }

    pub(crate) fn parse_metadata_file(
        &self,
        at: &Path,
    ) -> Result<CompileResult, SpirvBuilderError> {
        let metadata_contents = File::open(at).map_err(SpirvBuilderError::MetadataFileMissing)?;
        // FIXME(eddyb) move this functionality into `rustc_codegen_spirv_types`.
        let metadata: CompileResult =
            rustc_codegen_spirv_types::serde_json::from_reader(BufReader::new(metadata_contents))
                .map_err(SpirvBuilderError::MetadataFileMalformed)?;
        match &metadata.module {
            ModuleResult::SingleModule(spirv_module) => {
                assert!(!self.multimodule);
                let env_var = format!(
                    "{}.spv",
                    at.file_name()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .strip_suffix(ARTIFACT_SUFFIX)
                        .unwrap()
                );
                if self.print_metadata == MetadataPrintout::Full {
                    println!("cargo:rustc-env={}={}", env_var, spirv_module.display());
                }
            }
            ModuleResult::MultiModule(_) => {
                assert!(self.multimodule);
            }
        }
        Ok(metadata)
    }
}

// https://github.com/rust-lang/cargo/blob/1857880b5124580c4aeb4e8bc5f1198f491d61b1/src/cargo/util/paths.rs#L29-L52
fn dylib_path_envvar() -> &'static str {
    if cfg!(windows) {
        "PATH"
    } else if cfg!(target_os = "macos") {
        "DYLD_FALLBACK_LIBRARY_PATH"
    } else {
        "LD_LIBRARY_PATH"
    }
}
fn dylib_path() -> Vec<PathBuf> {
    let mut dylibs = match env::var_os(dylib_path_envvar()) {
        Some(var) => env::split_paths(&var).collect(),
        None => Vec::new(),
    };
    if let Ok(dir) = env::current_dir() {
        dylibs.push(dir);
    }
    dylibs
}

fn find_rustc_codegen_spirv() -> Result<PathBuf, SpirvBuilderError> {
    if cfg!(feature = "rustc_codegen_spirv") {
        let filename = format!(
            "{}rustc_codegen_spirv{}",
            env::consts::DLL_PREFIX,
            env::consts::DLL_SUFFIX
        );
        let dylib_paths = dylib_path();
        for mut path in dylib_paths {
            path.push(&filename);
            if path.is_file() {
                return Ok(path);
            }
        }
        panic!("Could not find {filename} in library path");
    } else {
        Err(SpirvBuilderError::MissingRustcCodegenSpirvDylib)
    }
}

/// Joins strings together while ensuring none of the strings contain the separator.
// NOTE(eddyb) this intentionally consumes the `Vec` to limit accidental misuse.
fn join_checking_for_separators(strings: Vec<impl Borrow<str>>, sep: &str) -> String {
    for s in &strings {
        let s = s.borrow();
        assert!(!s.contains(sep), "{s:?} may not contain separator {sep:?}");
    }
    strings.join(sep)
}

// Returns path to the metadata json.
fn invoke_rustc(builder: &SpirvBuilder) -> Result<PathBuf, SpirvBuilderError> {
    let path_to_crate = builder
        .path_to_crate
        .as_ref()
        .ok_or(SpirvBuilderError::MissingCratePath)?;
    let target_env;
    {
        let target = builder
            .target
            .as_ref()
            .ok_or(SpirvBuilderError::MissingTarget)?;
        target_env = target.strip_prefix(SPIRV_TARGET_PREFIX).ok_or_else(|| {
            SpirvBuilderError::NonSpirvTarget {
                target: target.clone(),
            }
        })?;
        // HACK(eddyb) used only to split the full list into groups.
        #[allow(clippy::match_same_arms)]
        match target_env {
            // HACK(eddyb) hardcoded list to avoid checking if the JSON file
            // for a particular target exists (and sanitizing strings for paths).
            //
            // FIXME(eddyb) consider moving this list, or even `target-specs`,
            // into `rustc_codegen_spirv_types`'s code/source.
            "spv1.0" | "spv1.1" | "spv1.2" | "spv1.3" | "spv1.4" | "spv1.5" | "spv1.6" => {}
            "opengl4.0" | "opengl4.1" | "opengl4.2" | "opengl4.3" | "opengl4.5" => {}
            "vulkan1.0" | "vulkan1.1" | "vulkan1.1spv1.4" | "vulkan1.2" | "vulkan1.3"
            | "vulkan1.4" => {}

            _ => {
                return Err(SpirvBuilderError::UnsupportedSpirvTargetEnv {
                    target_env: target_env.into(),
                });
            }
        }

        if (builder.print_metadata == MetadataPrintout::Full) && builder.multimodule {
            return Err(SpirvBuilderError::MultiModuleWithPrintMetadata);
        }
        if !path_to_crate.is_dir() {
            return Err(SpirvBuilderError::CratePathDoesntExist(
                path_to_crate.clone(),
            ));
        }
    }

    let toolchain_rustc_version =
        if let Some(toolchain_rustc_version) = &builder.toolchain_rustc_version {
            toolchain_rustc_version.clone()
        } else {
            query_rustc_version(builder.toolchain_overwrite.as_deref())?
        };

    // Okay, this is a little bonkers: in a normal world, we'd have the user clone
    // rustc_codegen_spirv and pass in the path to it, and then we'd invoke cargo to build it, grab
    // the resulting .so, and pass it into -Z codegen-backend. But that's really gross: the user
    // needs to clone rustc_codegen_spirv and tell us its path! So instead, we *directly reference
    // rustc_codegen_spirv in spirv-builder's Cargo.toml*, which means that it will get built
    // alongside build.rs, and cargo will helpfully add it to LD_LIBRARY_PATH for us! However,
    // rustc expects a full path, instead of a filename looked up via LD_LIBRARY_PATH, so we need
    // to copy cargo's understanding of library lookup and find the library and its full path.
    let rustc_codegen_spirv = Ok(builder.rustc_codegen_spirv_location.clone())
        .transpose()
        .unwrap_or_else(find_rustc_codegen_spirv)?;
    if !rustc_codegen_spirv.is_file() {
        return Err(SpirvBuilderError::RustcCodegenSpirvDylibDoesNotExist(
            rustc_codegen_spirv,
        ));
    }

    let mut rustflags = vec![
        format!("-Zcodegen-backend={}", rustc_codegen_spirv.display()),
        // Ensure the codegen backend is emitted in `.d` files to force Cargo
        // to rebuild crates compiled with it when it changes (this used to be
        // the default until https://github.com/rust-lang/rust/pull/93969).
        "-Zbinary-dep-depinfo".to_string(),
        "-Csymbol-mangling-version=v0".to_string(),
        "-Zcrate-attr=feature(register_tool)".to_string(),
        "-Zcrate-attr=register_tool(rust_gpu)".to_string(),
        // HACK(eddyb) this is the same configuration that we test with, and
        // ensures no unwanted surprises from e.g. `core` debug assertions.
        "-Coverflow-checks=off".to_string(),
        "-Cdebug-assertions=off".to_string(),
        // HACK(eddyb) we need this for `core::fmt::rt::Argument::new_*` calls
        // to *never* be inlined, so we can pattern-match the calls themselves.
        "-Zinline-mir=off".to_string(),
        // HACK(eddyb) similar to turning MIR inlining off, we also can't allow
        // optimizations that drastically impact (the quality of) codegen, and
        // GVN currently can lead to the memcpy-out-of-const-alloc-global-var
        // pattern, even for `ScalarPair` (e.g. `return None::<u32>;`).
        "-Zmir-enable-passes=-GVN".to_string(),
        // HACK(eddyb) avoid ever reusing instantiations from `compiler_builtins`
        // which is special-cased to turn calls to functions that never return,
        // into aborts, and this applies to the panics of UB-checking helpers
        // (https://github.com/rust-lang/rust/pull/122580#issuecomment-3033026194)
        // but while upstream that only loses the panic message, for us it's even
        // worse, as we lose the chance to remove otherwise-dead `fmt::Arguments`.
        "-Zshare-generics=off".to_string(),
    ];

    // Wrapper for `env::var` that appropriately informs Cargo of the dependency.
    let tracked_env_var_get = |name| {
        if let MetadataPrintout::Full | MetadataPrintout::DependencyOnly = builder.print_metadata {
            println!("cargo:rerun-if-env-changed={name}");
        }
        env::var(name)
    };

    let mut llvm_args = vec![];
    if builder.multimodule {
        llvm_args.push("--module-output=multiple".to_string());
    }
    match builder.spirv_metadata {
        SpirvMetadata::None => (),
        SpirvMetadata::NameVariables => {
            llvm_args.push("--spirv-metadata=name-variables".to_string());
        }
        SpirvMetadata::Full => llvm_args.push("--spirv-metadata=full".to_string()),
    }
    if builder.validator.relax_struct_store {
        llvm_args.push("--relax-struct-store".to_string());
    }
    if builder.validator.relax_logical_pointer {
        llvm_args.push("--relax-logical-pointer".to_string());
    }
    if builder.validator.relax_block_layout.unwrap_or(false) {
        llvm_args.push("--relax-block-layout".to_string());
    }
    if builder.validator.uniform_buffer_standard_layout {
        llvm_args.push("--uniform-buffer-standard-layout".to_string());
    }
    if builder.validator.scalar_block_layout {
        llvm_args.push("--scalar-block-layout".to_string());
    }
    if builder.validator.skip_block_layout {
        llvm_args.push("--skip-block-layout".to_string());
    }
    if builder.optimizer.preserve_bindings {
        llvm_args.push("--preserve-bindings".to_string());
    }
    let mut target_features = vec![];
    let abort_strategy = match builder.shader_panic_strategy {
        ShaderPanicStrategy::SilentExit => None,
        ShaderPanicStrategy::DebugPrintfThenExit {
            print_inputs,
            print_backtrace,
        } => {
            target_features.push("+ext:SPV_KHR_non_semantic_info".into());
            Some(format!(
                "debug-printf{}{}",
                if print_inputs { "+inputs" } else { "" },
                if print_backtrace { "+backtrace" } else { "" }
            ))
        }
        ShaderPanicStrategy::UNSOUND_DO_NOT_USE_UndefinedBehaviorViaUnreachable => {
            Some("unreachable".into())
        }
    };
    llvm_args.extend(abort_strategy.map(|strategy| format!("--abort-strategy={strategy}")));

    if let Ok(extra_codegen_args) = tracked_env_var_get("RUSTGPU_CODEGEN_ARGS") {
        llvm_args.extend(extra_codegen_args.split_whitespace().map(|s| s.to_string()));
    } else {
        llvm_args.extend(builder.extra_args.iter().cloned());
    }

    let llvm_args = join_checking_for_separators(llvm_args, " ");
    if !llvm_args.is_empty() {
        rustflags.push(["-Cllvm-args=", &llvm_args].concat());
    }

    target_features.extend(builder.capabilities.iter().map(|cap| format!("+{cap:?}")));
    target_features.extend(builder.extensions.iter().map(|ext| format!("+ext:{ext}")));
    let target_features = join_checking_for_separators(target_features, ",");
    if !target_features.is_empty() {
        rustflags.push(["-Ctarget-feature=", &target_features].concat());
    }

    if builder.deny_warnings {
        rustflags.push("-Dwarnings".to_string());
    }

    if let Ok(extra_rustflags) = tracked_env_var_get("RUSTGPU_RUSTFLAGS") {
        rustflags.extend(extra_rustflags.split_whitespace().map(|s| s.to_string()));
    }

    let target_dir_path = builder
        .target_dir_path
        .clone()
        .unwrap_or_else(|| PathBuf::from("spirv-builder"));
    let target_dir = if target_dir_path.is_absolute() {
        target_dir_path
    } else {
        let metadata = cargo_metadata::MetadataCommand::new()
            .current_dir(path_to_crate)
            .exec()?;
        metadata
            .target_directory
            .into_std_path_buf()
            .join(target_dir_path)
    };

    let mut cargo = cargo_cmd::CargoCmd::new();
    if let Some(toolchain) = &builder.toolchain_overwrite {
        cargo.arg(format!("+{toolchain}"));
    }

    let cargo_cmd = builder.cargo_cmd.as_ref().map_or("rustc", |s| s.as_str());
    let cargo_cmd_like_rustc = builder.cargo_cmd_like_rustc.unwrap_or(cargo_cmd == "rustc");
    let profile = if builder.release { "release" } else { "dev" };
    cargo.args([
        cargo_cmd,
        "--lib",
        "--message-format=json-render-diagnostics",
        "-Zbuild-std=core",
        "-Zbuild-std-features=compiler-builtins-mem",
        "--profile",
        profile,
    ]);
    if cargo_cmd_like_rustc {
        // About `crate-type`: We use it to determine whether the crate needs to be linked into shaders. For `rlib`,
        // we're emitting regular rust libraries as is expected. For `dylib` or `cdylib`, we're linking all `rlib`s
        // together, legalize them in many passes and emit a final `*.spv` file. Quirk: If you depend on a crate
        // that has crate-type `dylib`, we also link it, and it will fail if it has no shaders, which may not be
        // desired. (Gathered from reading source code and experimenting, @firestar99)
        cargo.args(["--crate-type", "dylib"]);
    }

    if let Ok(extra_cargoflags) = tracked_env_var_get("RUSTGPU_CARGOFLAGS") {
        cargo.args(extra_cargoflags.split_whitespace());
    }

    let target_spec_dir = target_dir.join("target-specs");
    let target =
        TargetSpecVersion::target_arg(toolchain_rustc_version, target_env, &target_spec_dir)?;
    cargo.arg("--target").arg(target);

    if !builder.shader_crate_features.default_features {
        cargo.arg("--no-default-features");
    }

    if !builder.shader_crate_features.features.is_empty() {
        cargo
            .arg("--features")
            .arg(builder.shader_crate_features.features.join(","));
    }

    cargo.arg("--target-dir").arg(target_dir);

    // NOTE(eddyb) this used to be just `RUSTFLAGS` but at some point Cargo
    // added a separate environment variable using `\x1f` instead of spaces,
    // which allows us to have spaces within individual `rustc` flags.
    cargo.env(
        "CARGO_ENCODED_RUSTFLAGS",
        join_checking_for_separators(rustflags, "\x1f"),
    );

    // NOTE(eddyb) there's no parallelism to take advantage of multiple CGUs,
    // and inter-CGU duplication can be wasteful, so this forces 1 CGU for now.
    let profile_in_env_var = profile.replace('-', "_").to_ascii_uppercase();
    let num_cgus = 1;
    cargo.env(
        format!("CARGO_PROFILE_{profile_in_env_var}_CODEGEN_UNITS"),
        num_cgus.to_string(),
    );

    cargo.stderr(Stdio::inherit()).current_dir(path_to_crate);
    log::debug!("building shaders with `{cargo:?}`");
    let build = cargo.output().expect("failed to execute cargo build");

    // `get_last_artifact` has the side-effect of printing invalid lines, so
    // we do that even in case of an error, to let through any useful messages
    // that ended up on stdout instead of stderr.
    let stdout = String::from_utf8(build.stdout).unwrap();
    if build.status.success() {
        get_sole_artifact(&stdout).ok_or(SpirvBuilderError::NoArtifactProduced { stdout })
    } else {
        Err(SpirvBuilderError::BuildFailed)
    }
}

#[derive(Deserialize)]
struct RustcOutput {
    reason: String,
    filenames: Option<Vec<String>>,
}

const ARTIFACT_SUFFIX: &str = ".spv.json";

fn get_sole_artifact(out: &str) -> Option<PathBuf> {
    let mut last_compiler_artifact = None;
    for line in out.lines() {
        let Ok(msg) = serde_json::from_str::<RustcOutput>(line) else {
            // Pass through invalid lines
            println!("{line}");
            continue;
        };
        if msg.reason == "compiler-artifact" {
            last_compiler_artifact = Some(msg);
        }
    }
    let last_compiler_artifact =
        last_compiler_artifact.expect("Did not find output file in rustc output");

    let mut filenames = last_compiler_artifact
        .filenames
        .unwrap()
        .into_iter()
        .filter(|v| v.ends_with(ARTIFACT_SUFFIX));
    let filename = filenames.next()?;
    assert_eq!(
        filenames.next(),
        None,
        "build had multiple `{ARTIFACT_SUFFIX}` artifacts"
    );
    Some(filename.into())
}

/// Internally iterate through the leaf dependencies of the artifact at `artifact`
fn leaf_deps(artifact: &Path, mut handle: impl FnMut(&RawStr)) -> std::io::Result<()> {
    let deps_file = artifact.with_extension("d");
    let mut deps_map = HashMap::new();
    depfile::read_deps_file(&deps_file, |item, deps| {
        deps_map.insert(item, deps);
        Ok(())
    })?;
    fn recurse(
        map: &HashMap<RawString, Vec<RawString>>,
        artifact: &RawStr,
        handle: &mut impl FnMut(&RawStr),
    ) {
        match map.get(artifact) {
            Some(entries) => {
                for entry in entries {
                    recurse(map, entry, handle);
                }
            }
            None => handle(artifact),
        }
    }
    recurse(&deps_map, artifact.to_str().unwrap().into(), &mut handle);
    Ok(())
}
