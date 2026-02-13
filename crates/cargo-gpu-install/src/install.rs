//! Install a dedicated per-shader crate that has the `rust-gpu` compiler in it.

use crate::spirv_source::{
    get_channel_from_rustc_codegen_spirv_build_script, query_metadata, FindPackage as _,
};
use crate::{cache_dir, spirv_source::SpirvSource};
use anyhow::Context as _;
use spirv_builder::SpirvBuilder;
use std::path::{Path, PathBuf};

/// Represents a functional backend installation, whether it was cached or just installed.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct InstalledBackend {
    /// path to the `rustc_codegen_spirv` dylib
    pub rustc_codegen_spirv_location: PathBuf,
    /// toolchain channel name
    pub toolchain_channel: String,
}

impl InstalledBackend {
    /// Creates a new `SpirvBuilder` configured to use this installed backend.
    #[expect(
        clippy::unreachable,
        reason = "it's unreachable, no need to return a Result"
    )]
    #[expect(clippy::impl_trait_in_params, reason = "forwarding spirv-builder API")]
    #[inline]
    pub fn to_spirv_builder(
        &self,
        path_to_crate: impl AsRef<Path>,
        target: impl Into<String>,
    ) -> SpirvBuilder {
        let mut builder = SpirvBuilder::new(path_to_crate, target);
        self.configure_spirv_builder(&mut builder)
            .unwrap_or_else(|_| unreachable!("we set target before calling this function"));
        builder
    }

    /// Configures the supplied [`SpirvBuilder`]. `SpirvBuilder.target` must be set and must not change after calling this function.
    ///
    /// # Errors
    /// if `SpirvBuilder.target` is not set
    #[inline]
    pub fn configure_spirv_builder(&self, builder: &mut SpirvBuilder) -> anyhow::Result<()> {
        builder.rustc_codegen_spirv_location = Some(self.rustc_codegen_spirv_location.clone());
        builder.toolchain_overwrite = Some(self.toolchain_channel.clone());
        Ok(())
    }
}

/// Args for an install
#[expect(
    clippy::struct_excessive_bools,
    reason = "cmdline args have many bools"
)]
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[non_exhaustive]
pub struct Install {
    /// Cargo target to compile.
    ///
    /// Conflicts with `--shader-crate`.
    #[cfg_attr(feature = "clap", clap(short, long, conflicts_with("shader_crate")))]
    pub package: Option<String>,

    /// Directory containing the shader crate to compile.
    ///
    /// Conflicts with `--package` or `-p`.
    #[cfg_attr(
        feature = "clap",
        clap(long, default_value = "./", conflicts_with("package"))
    )]
    pub shader_crate: PathBuf,

    #[expect(
        clippy::doc_markdown,
        reason = "The URL should appear literally like this. But Clippy wants a markdown clickable link"
    )]
    /// Source of `spirv-builder` dependency
    /// Eg: "https://github.com/Rust-GPU/rust-gpu"
    #[cfg_attr(feature = "clap", clap(long))]
    pub spirv_builder_source: Option<String>,

    /// Version of `spirv-builder` dependency.
    /// * If `--spirv-builder-source` is not set, then this is assumed to be a crates.io semantic
    ///   version such as "0.9.0".
    /// * If `--spirv-builder-source` is set, then this is assumed to be a Git "commitsh", such
    ///   as a Git commit hash or a Git tag, therefore anything that `git checkout` can resolve.
    #[cfg_attr(feature = "clap", clap(long, verbatim_doc_comment))]
    pub spirv_builder_version: Option<String>,

    /// Force `rustc_codegen_spirv` to be rebuilt.
    #[cfg_attr(feature = "clap", clap(long))]
    pub rebuild_codegen: bool,

    /// Assume "yes" to "Install Rust toolchain: [y/n]" prompt.
    ///
    /// Defaults to `false` in cli, `true` in [`Default`]
    #[cfg_attr(feature = "clap", clap(long, action))]
    pub auto_install_rust_toolchain: bool,

    /// Clear target dir of `rustc_codegen_spirv` build after a successful build, saves about
    /// 200MiB of disk space.
    #[cfg_attr(feature = "clap", clap(long = "no-clear-target", default_value = "true", action = clap::ArgAction::SetFalse))]
    pub clear_target: bool,

    /// There is a tricky situation where a shader crate that depends on workspace config can have
    /// a different `Cargo.lock` lockfile version from the the workspace's `Cargo.lock`. This can
    /// prevent builds when an old Rust toolchain doesn't recognise the newer lockfile version.
    ///
    /// The ideal way to resolve this would be to match the shader crate's toolchain with the
    /// workspace's toolchain. However, that is not always possible. Another solution is to
    /// `exclude = [...]` the problematic shader crate from the workspace. This also may not be a
    /// suitable solution if there are a number of shader crates all sharing similar config and
    /// you don't want to have to copy/paste and maintain that config across all the shaders.
    ///
    /// So a somewhat hacky workaround is to have `cargo gpu` overwrite lockfile versions. Enabling
    /// this flag will only come into effect if there are a mix of v3/v4 lockfiles. It will also
    /// only overwrite versions for the duration of a build. It will attempt to return the versions
    /// to their original values once the build is finished. However, of course, unexpected errors
    /// can occur and the overwritten values can remain. Hence why this behaviour is not enabled by
    /// default.
    ///
    /// This hack is possible because the change from v3 to v4 only involves a minor change to the
    /// way source URLs are encoded. See these PRs for more details:
    ///   * <https://github.com/rust-lang/cargo/pull/12280>
    ///   * <https://github.com/rust-lang/cargo/pull/14595>
    #[cfg_attr(feature = "clap", clap(long, action, verbatim_doc_comment))]
    pub force_overwrite_lockfiles_v4_to_v3: bool,
}

impl Install {
    /// Create a default install for a shader crate of some path
    #[inline]
    #[must_use]
    pub const fn from_shader_crate(shader_crate: PathBuf) -> Self {
        Self {
            package: None,
            shader_crate,
            spirv_builder_source: None,
            spirv_builder_version: None,
            rebuild_codegen: false,
            auto_install_rust_toolchain: true,
            clear_target: true,
            force_overwrite_lockfiles_v4_to_v3: false,
        }
    }

    /// Create the `rustc_codegen_spirv_dummy` crate that depends on `rustc_codegen_spirv`
    fn write_source_files(source: &SpirvSource, checkout: &Path) -> anyhow::Result<()> {
        // skip writing a dummy project if we use a local rust-gpu checkout
        if source.is_path() {
            return Ok(());
        }
        log::debug!(
            "writing `rustc_codegen_spirv_dummy` source files into '{}'",
            checkout.display()
        );

        {
            log::trace!("writing dummy lib.rs");
            let src = checkout.join("src");
            std::fs::create_dir_all(&src).context("creating 'src' directory")?;
            std::fs::File::create(src.join("lib.rs")).context("creating 'src/lib.rs'")?;
        };

        {
            log::trace!("writing dummy Cargo.toml");
            let version_spec = match &source {
                SpirvSource::CratesIO(version) => {
                    format!("version = \"{version}\"")
                }
                SpirvSource::Git { url, rev } => format!("git = \"{url}\"\nrev = \"{rev}\""),
                SpirvSource::Path {
                    rust_gpu_repo_root,
                    version,
                } => {
                    // this branch is currently unreachable, as we just build `rustc_codegen_spirv` directly,
                    // since we don't need the `dummy` crate to make cargo download it for us
                    let mut new_path = rust_gpu_repo_root.to_owned();
                    new_path.push("crates/spirv-builder");
                    format!("path = \"{new_path}\"\nversion = \"{version}\"")
                }
            };
            let cargo_toml = format!(
                r#"
[package]
name = "rustc_codegen_spirv_dummy"
version = "0.1.0"
edition = "2021"

[dependencies.spirv-builder]
package = "rustc_codegen_spirv"
{version_spec}
            "#
            );
            std::fs::write(checkout.join("Cargo.toml"), cargo_toml)
                .context("writing 'Cargo.toml'")?;
        };
        Ok(())
    }

    /// Install the binary pair and return the [`InstalledBackend`], from which you can create [`SpirvBuilder`] instances.
    ///
    /// # Errors
    /// If the installation somehow fails.
    #[inline]
    #[expect(clippy::too_many_lines, reason = "it's fine")]
    pub fn run(&self) -> anyhow::Result<InstalledBackend> {
        // Ensure the cache dir exists
        let cache_dir = cache_dir()?;
        log::info!("cache directory is '{}'", cache_dir.display());
        std::fs::create_dir_all(&cache_dir).with_context(|| {
            format!("could not create cache directory '{}'", cache_dir.display())
        })?;

        let source = SpirvSource::new(
            &self.shader_crate,
            self.spirv_builder_source.as_deref(),
            self.spirv_builder_version.as_deref(),
        )?;
        let install_dir = source.install_dir()?;

        let dylib_filename = format!(
            "{}rustc_codegen_spirv{}",
            std::env::consts::DLL_PREFIX,
            std::env::consts::DLL_SUFFIX
        );

        let (dest_dylib_path, skip_rebuild) = if source.is_path() {
            (
                install_dir
                    .join("target")
                    .join("release")
                    .join(&dylib_filename),
                // if `source` is a path, always rebuild
                false,
            )
        } else {
            let dest_dylib_path = install_dir.join(&dylib_filename);
            let artifacts_found = dest_dylib_path.is_file()
                && install_dir.join("Cargo.toml").is_file()
                && install_dir.join("src").join("lib.rs").is_file();
            if artifacts_found {
                log::info!("cargo-gpu artifacts found in '{}'", install_dir.display());
            }
            (dest_dylib_path, artifacts_found && !self.rebuild_codegen)
        };

        if skip_rebuild {
            log::info!("...and so we are aborting the install step.");
        } else {
            Self::write_source_files(&source, &install_dir).context("writing source files")?;
        }

        // TODO cache toolchain channel in a file?
        log::debug!("resolving toolchain version to use");
        let dummy_metadata = query_metadata(&install_dir)
            .context("resolving toolchain version: get `rustc_codegen_spirv_dummy` metadata")?;
        let rustc_codegen_spirv = dummy_metadata.find_package("rustc_codegen_spirv").context(
            "resolving toolchain version: expected a dependency on `rustc_codegen_spirv`",
        )?;
        let toolchain_channel =
            get_channel_from_rustc_codegen_spirv_build_script(rustc_codegen_spirv).context(
                "resolving toolchain version: read toolchain from `rustc_codegen_spirv`'s build.rs",
            )?;
        log::info!("selected toolchain channel `{toolchain_channel:?}`");

        log::debug!("ensure_toolchain_and_components_exist");
        crate::install_toolchain::ensure_toolchain_and_components_exist(
            &toolchain_channel,
            self.auto_install_rust_toolchain,
        )
        .context("ensuring toolchain and components exist")?;

        if !skip_rebuild {
            // to prevent unsupported version errors when using older toolchains
            if !source.is_path() {
                log::debug!("remove Cargo.lock");
                std::fs::remove_file(install_dir.join("Cargo.lock"))
                    .context("remove Cargo.lock")?;
            }

            crate::user_output!("Compiling `rustc_codegen_spirv` from source {}\n", source);
            let mut cargo = spirv_builder::cargo_cmd::CargoCmd::new();
            // Make sure spirv-tools is build normally and does not skip C++ compile due to "being run in clippy"
            // We add this only to our install and not generally to `CargoCmd` since we do want to forward clippy args
            // to clippy running on the spirv target via e.g. `cargo gpu clippy`.
            cargo.env_remove("CLIPPY_ARGS");
            cargo
                .current_dir(&install_dir)
                .arg(format!("+{toolchain_channel}"))
                .args(["build", "--release"]);
            if source.is_path() {
                cargo.args(["-p", "rustc_codegen_spirv", "--lib"]);
            }

            log::debug!("building artifacts with `{cargo:?}`");
            cargo
                .stdout(std::process::Stdio::inherit())
                .stderr(std::process::Stdio::inherit())
                .output()
                .context("getting command output")
                .and_then(|output| {
                    if output.status.success() {
                        Ok(output)
                    } else {
                        Err(anyhow::anyhow!("bad status {:?}", output.status))
                    }
                })
                .context("running build command")?;

            let target = install_dir.join("target");
            let dylib_path = target.join("release").join(&dylib_filename);
            if dylib_path.is_file() {
                log::info!("successfully built {}", dylib_path.display());
                if !source.is_path() {
                    std::fs::rename(&dylib_path, &dest_dylib_path)
                        .context("renaming dylib path")?;

                    if self.clear_target {
                        log::warn!("clearing target dir {}", target.display());
                        std::fs::remove_dir_all(&target).context("clearing target dir")?;
                    }
                }
            } else {
                log::error!("could not find {}", dylib_path.display());
                anyhow::bail!("`rustc_codegen_spirv` build failed");
            }
        }

        Ok(InstalledBackend {
            rustc_codegen_spirv_location: dest_dylib_path,
            toolchain_channel,
        })
    }
}
