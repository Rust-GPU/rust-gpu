#![allow(clippy::shadow_reuse, reason = "let's not be silly")]
#![allow(clippy::unwrap_used, reason = "this is basically a test")]
//! `cargo gpu build`, analogous to `cargo build`

use crate::install::Install;
use crate::linkage::Linkage;
use crate::lockfile::LockfileMismatchHandler;
use crate::spirv_builder::{CompileResult, ModuleResult, SpirvBuilder, SpirvBuilderError};
use anyhow::Context as _;
use std::io::Write as _;
use std::path::PathBuf;

/// Args for just a build
#[derive(clap::Parser, Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct BuildArgs {
    /// Path to the output directory for the compiled shaders.
    #[clap(long, short, default_value = "./")]
    pub output_dir: PathBuf,

    /// Watch the shader crate directory and automatically recompile on changes.
    #[clap(long, short, action)]
    pub watch: bool,

    /// The flattened [`SpirvBuilder`]
    #[clap(flatten)]
    #[serde(flatten)]
    pub spirv_builder: SpirvBuilder,

    /// Renames the `manifest.json` file to the given name
    #[clap(long, short, default_value = "manifest.json")]
    pub manifest_file: String,

    /// When building fails with [`SpirvBuilderError::NoArtifactProduced`], count it as a success anyway.
    /// Used for e.g. `clippy`, which doesn't produce any artifacts. Defaults to false.
    #[clap(skip)]
    pub allow_no_artifacts: bool,
}

impl Default for BuildArgs {
    #[inline]
    fn default() -> Self {
        Self {
            output_dir: PathBuf::from("./"),
            watch: false,
            spirv_builder: SpirvBuilder::default(),
            manifest_file: String::from("manifest.json"),
            allow_no_artifacts: false,
        }
    }
}

/// `cargo build` subcommands
#[derive(Clone, clap::Parser, Debug, serde::Deserialize, serde::Serialize)]
pub struct Build {
    /// CLI args for install the `rust-gpu` compiler and components
    #[clap(flatten)]
    pub install: Install,

    /// CLI args for configuring the build of the shader
    #[clap(flatten)]
    pub build: BuildArgs,
}

impl Build {
    /// Entrypoint
    pub fn run(&mut self) -> anyhow::Result<()> {
        let installed_backend = self.install.run()?;
        let mut metadata = crate::metadata::MetadataCache::default();

        if let Some(package) = self.install.package.as_ref() {
            self.install.shader_crate = metadata.resolve_package_to_shader_crate(package)?;
        }

        let _lockfile_mismatch_handler = LockfileMismatchHandler::new(
            &self.install.shader_crate,
            &installed_backend.toolchain_channel,
            self.install.force_overwrite_lockfiles_v4_to_v3,
        )?;

        let builder = &mut self.build.spirv_builder;
        builder.path_to_crate = Some(self.install.shader_crate.clone());
        installed_backend.configure_spirv_builder(builder)?;

        // Ensure the shader output dir exists
        log::debug!(
            "ensuring output-dir '{}' exists",
            self.build.output_dir.display()
        );
        std::fs::create_dir_all(&self.build.output_dir)?;
        let canonicalized = dunce::canonicalize(&self.build.output_dir)?;
        log::debug!("canonicalized output dir: {}", canonicalized.display());
        self.build.output_dir = canonicalized;

        // Ensure the shader crate exists
        self.install.shader_crate = dunce::canonicalize(&self.install.shader_crate)?;
        anyhow::ensure!(
            self.install.shader_crate.exists(),
            "shader crate '{}' does not exist. (Current dir is '{}')",
            self.install.shader_crate.display(),
            std::env::current_dir()?.display()
        );

        if self.build.watch {
            let mut watcher = self.build.spirv_builder.clone().watch()?;
            loop {
                // if the build fails "regularly", eg. `cargo build` fails and nothing else, just retry
                crate::user_output!(
                    "Compiling shaders at {}...\n",
                    self.install.shader_crate.display()
                );
                match watcher.recv() {
                    Ok(result) => {
                        self.parse_compilation_result(&result)?;
                        crate::user_output!("Build successful!\n");
                    }
                    Err(SpirvBuilderError::BuildFailed) => crate::user_output!("Build failed!\n"),
                    Err(err) => return Err(anyhow::Error::from(err)),
                }
            }
        } else {
            crate::user_output!(
                "Compiling shaders at {}...\n",
                self.install.shader_crate.display()
            );
            let result = self.build.spirv_builder.build();
            match result {
                Ok(result) => {
                    self.parse_compilation_result(&result)?;
                }
                // conditionally ignore NoArtifactProduced
                Err(SpirvBuilderError::NoArtifactProduced { .. })
                    if self.build.allow_no_artifacts => {}
                Err(err) => return Err(err.into()),
            }
        }
        Ok(())
    }

    /// Parses compilation result from `SpirvBuilder` and writes it out to a file
    fn parse_compilation_result(&self, result: &CompileResult) -> anyhow::Result<()> {
        let shaders = match &result.module {
            ModuleResult::MultiModule(modules) => {
                anyhow::ensure!(!modules.is_empty(), "No shader modules were compiled");
                modules.iter().collect::<Vec<_>>()
            }
            ModuleResult::SingleModule(filepath) => result
                .entry_points
                .iter()
                .map(|entry| (entry, filepath))
                .collect::<Vec<_>>(),
        };
        let mut linkage: Vec<Linkage> = shaders
            .into_iter()
            .map(|(entry, filepath)| -> anyhow::Result<Linkage> {
                use relative_path::PathExt as _;
                let path = self.build.output_dir.join(
                    filepath
                        .file_name()
                        .context("Couldn't parse file name from shader module path")?,
                );
                log::debug!("copying {} to {}", filepath.display(), path.display());
                std::fs::copy(filepath, &path)?;
                log::debug!(
                    "linkage of {} relative to {}",
                    path.display(),
                    self.install.shader_crate.display()
                );
                let spv_path = path
                    .relative_to(&self.install.shader_crate)
                    .map_or(path, |path_relative_to_shader_crate| {
                        path_relative_to_shader_crate.to_path("")
                    });
                Ok(Linkage::new(entry, spv_path))
            })
            .collect::<anyhow::Result<Vec<Linkage>>>()?;
        // Sort the contents so the output is deterministic
        linkage.sort();

        // Write the shader manifest json file
        let manifest_path = self.build.output_dir.join(&self.build.manifest_file);
        let json = serde_json::to_string_pretty(&linkage)?;
        let mut file = std::fs::File::create(&manifest_path).with_context(|| {
            format!(
                "could not create shader manifest file '{}'",
                manifest_path.display(),
            )
        })?;
        file.write_all(json.as_bytes()).with_context(|| {
            format!(
                "could not write shader manifest file '{}'",
                manifest_path.display(),
            )
        })?;

        log::info!("wrote manifest to '{}'", manifest_path.display());
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use clap::Parser as _;

    use crate::{Cli, Command};

    #[test_log::test]
    fn builder_from_params() {
        let shader_crate_path = crate::test::shader_crate_template_path();
        let output_dir = shader_crate_path.join("shaders");

        let args = [
            "target/debug/cargo-gpu",
            "build",
            "--shader-crate",
            &format!("{}", shader_crate_path.display()),
            "--output-dir",
            &format!("{}", output_dir.display()),
        ];
        if let Cli {
            command: Command::Build(build),
        } = Cli::parse_from(args)
        {
            assert_eq!(shader_crate_path, build.install.shader_crate);
            assert_eq!(output_dir, build.build.output_dir);

            // TODO:
            // For some reason running a full build (`build.run()`) inside tests fails on Windows.
            // The error is in the `build.rs` step of compiling `spirv-tools-sys`. It is not clear
            // from the logged error what the problem is. For now we'll just run a full build
            // outside the tests environment, see `xtask`'s `test-build`.
        } else {
            panic!("was not a build command");
        }
    }
}
