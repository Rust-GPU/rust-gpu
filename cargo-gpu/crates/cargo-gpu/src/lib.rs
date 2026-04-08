#![expect(clippy::pub_use, reason = "reexports from cargo_gpu_install crate")]

//! Rust GPU shader crate builder.
//!
//! This program and library allows you to easily compile your rust-gpu shaders,
//! without requiring you to fix your entire project to a specific toolchain.
//!
//! # How it works
//!
//! This program primarily manages installations of `rustc_codegen_spirv`, the
//! codegen backend of rust-gpu to generate SPIR-V shader binaries. The codegen
//! backend builds on internal, ever-changing interfaces of rustc, which requires
//! fixing a version of rust-gpu to a specific version of the rustc compiler.
//! Usually, this would require you to fix your entire project to that specific
//! toolchain, but this project loosens that requirement by managing installations
//! of `rustc_codegen_spirv` and their associated toolchains for you.
//!
//! We continue to use rust-gpu's `spirv_builder` crate to pass the many additional
//! parameters required to configure rustc and our codegen backend, but provide you
//! with a toolchain agnostic version that you may use from stable rustc. And a
//! `cargo gpu` cmdline utility to simplify shader building even more.
//!
//! ## Where the binaries are
//!
//! We store our prebuild `rustc_spirv_builder` binaries in the default cache
//! directory of your OS:
//! * Windows: `C:/users/<user>/AppData/Local/rust-gpu`
//! * Mac: `~/Library/Caches/rust-gpu`
//! * Linux: `~/.cache/rust-gpu`
//!
//! ## How we build the backend
//!
//! * retrieve the version of rust-gpu you want to use based on the version of the
//!   `spirv-std` dependency in your shader crate.
//! * create a dummy project at `<cache_dir>/codegen/<version>/` that depends on
//!   `rustc_codegen_spirv`
//! * use `cargo metadata` to `cargo update` the dummy project, which downloads the
//!   `rustc_codegen_spirv` crate into cargo's cache, and retrieve the path to the
//!   download location.
//! * search for the required toolchain in `build.rs` of `rustc_codegen_spirv`
//! * build it with the required toolchain version
//! * copy out the binary and clean the target dir
//!
//! ## Building shader crates
//!
//! `cargo-gpu` takes a path to a shader crate to build, as well as a path to a directory
//! to put the compiled `spv` source files. It also takes a path to an output manifest
//! file where all shader entry points will be mapped to their `spv` source files. This
//! manifest file can be used by build scripts (`build.rs` files) to generate linkage or
//! conduct other post-processing, like converting the `spv` files into `wgsl` files,
//! for example.

#[cfg(test)]
pub use cargo_gpu_install::test;
pub use cargo_gpu_install::{cache_dir, install, spirv_builder, spirv_source, user_output};
pub use metadata::MetadataCache;

mod build;
mod config;
mod dump_usage;
mod linkage;
mod lockfile;
mod metadata;
mod show;

/// All of the available subcommands for `cargo gpu`
#[derive(clap::Subcommand)]
#[non_exhaustive]
pub enum Command {
    /// Install rust-gpu compiler artifacts.
    Install(Box<install::Install>),

    /// Compile a shader crate to SPIR-V.
    Build(Box<build::Build>),

    /// Run `cargo check` on the shader crate with a SPIR-V target without building the actual shaders
    Check(Box<build::Build>),

    /// Run clippy on a shader crate with a SPIR-V target
    Clippy(Box<build::Build>),

    /// Show some useful values.
    Show(show::Show),

    /// A hidden command that can be used to recursively print out all the subcommand help messages:
    ///   `cargo gpu dump-usage`
    /// Useful for updating the README.
    #[clap(hide(true))]
    DumpUsage,
}

impl Command {
    /// Runs the command
    ///
    /// # Errors
    /// Any errors during execution, usually printed to the user
    #[inline]
    pub fn run(
        &self,
        env_args: Vec<String>,
        metadata_cache: &mut metadata::MetadataCache,
    ) -> anyhow::Result<()> {
        match &self {
            Self::Install(install) => {
                let shader_crate_path = &install.shader_crate;
                let command = config::Config::clap_command_with_cargo_config(
                    shader_crate_path,
                    env_args,
                    metadata_cache,
                )?;
                log::debug!(
                    "installing with final merged arguments: {:#?}",
                    command.install
                );
                command.install.run()?;
            }
            Self::Build(build) | Self::Check(build) | Self::Clippy(build) => {
                let shader_crate_path = &build.install.shader_crate;
                let mut command = config::Config::clap_command_with_cargo_config(
                    shader_crate_path,
                    env_args,
                    metadata_cache,
                )?;
                #[expect(clippy::wildcard_enum_match_arm, reason = "unreachable")]
                match self {
                    Self::Check(_) => {
                        command.build.spirv_builder.cargo_cmd = Some("check".into());
                        command.build.allow_no_artifacts = true;
                    }
                    Self::Clippy(_) => {
                        command.build.spirv_builder.cargo_cmd = Some("clippy".into());
                        command.build.allow_no_artifacts = true;
                    }
                    _ => {}
                }
                log::debug!("building with final merged arguments: {command:#?}");

                if command.build.watch {
                    //  When watching, do one normal run to setup the `manifest.json` file.
                    command.build.watch = false;
                    command.run()?;
                    command.build.watch = true;
                }
                command.run()?;
            }
            Self::Show(show) => show.run()?,
            Self::DumpUsage => dump_usage::dump_full_usage_for_readme()?,
        }

        Ok(())
    }
}

/// the Cli struct representing the main cli
#[derive(clap::Parser)]
#[clap(author, version, about, subcommand_required = true)]
#[non_exhaustive]
pub struct Cli {
    /// The command to run.
    #[clap(subcommand)]
    pub command: Command,
}
