//! Project/repository utilities.
#![allow(
    clippy::shadow_reuse,
    clippy::unwrap_used,
    clippy::unwrap_in_result,
    reason = "This is just a workflow tool"
)]

use anyhow::Context as _;
use clap::Parser as _;
use std::borrow::Cow;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};

/// Path to the shader crate
const SHADER_CRATE_PATH: &str = "crates/shader-crate-template";

/// Our xtask commands.
#[derive(Debug, clap::Parser)]
enum Cli {
    /// Run a test build of the shader-crate-template project.
    TestBuild {
        /// Build using the specified version of `spirv-std`.
        #[clap(long)]
        rust_gpu_version: Option<String>,
        /// The version of glam to use
        #[clap(long)]
        glam_version: Option<String>,
    },
    /// Set a dependency in the shader-crate-template to some version
    SetDependency {
        /// the dependency to modify
        package: String,
        /// the version to set it to
        version: String,
        /// the git repo to use, if version is a commit rev
        #[clap(long)]
        git: Option<String>,
    },
    UpdateExpect,
    RustGpuRev {
        rev: String,
    },
}

/// run some cmd
fn cmd(args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> anyhow::Result<()> {
    let mut args = args.into_iter();
    let status = std::process::Command::new(args.next().context("no args")?.as_ref())
        .args(args)
        .status()
        .context("cmd failed")?;
    anyhow::ensure!(status.success());
    Ok(())
}

/// Overwrites a toml file's output-dir field, and reverts that on drop.
struct ShaderCrateTemplateCargoTomlWriter {
    /// Original string
    original_shader_crate_template_str: String,
    /// Original lockfile
    original_shader_crate_lock_file: String,
    /// Parsed toml table
    table: toml::Table,
    /// false will reset Cargo.toml when this is dropped
    persistent: bool,
}

impl Drop for ShaderCrateTemplateCargoTomlWriter {
    fn drop(&mut self) {
        if self.persistent {
            return;
        }

        log::info!("reverting overwrite of Cargo.toml");
        std::fs::write(
            format!("{SHADER_CRATE_PATH}/Cargo.toml"),
            &self.original_shader_crate_template_str,
        )
        .unwrap();
        log::info!("reverting overwrite of Cargo.lock");
        std::fs::write(
            format!("{SHADER_CRATE_PATH}/Cargo.lock"),
            &self.original_shader_crate_lock_file,
        )
        .unwrap();
    }
}

impl Default for ShaderCrateTemplateCargoTomlWriter {
    fn default() -> Self {
        Self::new(false)
    }
}

impl ShaderCrateTemplateCargoTomlWriter {
    /// Create a new one
    fn new(persistent: bool) -> Self {
        let original_shader_crate_template_str =
            std::fs::read_to_string(format!("{SHADER_CRATE_PATH}/Cargo.toml")).unwrap();
        let table = toml::from_str::<toml::Table>(&original_shader_crate_template_str).unwrap();
        let original_shader_crate_lock_file =
            std::fs::read_to_string(format!("{SHADER_CRATE_PATH}/Cargo.lock")).unwrap();
        Self {
            original_shader_crate_template_str,
            original_shader_crate_lock_file,
            table,
            persistent,
        }
    }

    /// Get the `[dependencies]` section of the shader's `Cargo.toml`.
    fn get_cargo_dependencies_table(&mut self) -> &mut toml::Table {
        self.table
            .get_mut("dependencies")
            .unwrap()
            .as_table_mut()
            .unwrap()
    }

    /// Get the `[package.metadata.rust-gpu.build]` section of the shader's `Cargo.toml`.
    fn get_rust_gpu_table(&mut self) -> &mut toml::Table {
        let package = self
            .table
            .get_mut("package")
            .unwrap()
            .as_table_mut()
            .unwrap();
        let metadata = package.get_mut("metadata").unwrap().as_table_mut().unwrap();
        metadata
            .get_mut("rust-gpu")
            .unwrap()
            .as_table_mut()
            .unwrap()
    }

    /// Write any temporary changes to the shader crate's `Cargo.toml` that are needed to run e2e
    /// tests.
    fn write_shader_crate_cargo_toml_changes(&self) -> anyhow::Result<()> {
        std::fs::write(
            format!("{SHADER_CRATE_PATH}/Cargo.toml"),
            toml::to_string_pretty(&self.table).context("could not serialize")?,
        )
        .context("could not overwrite path")?;
        Ok(())
    }

    /// Replace the output-dir
    fn replace_output_dir(&mut self, path: impl AsRef<std::path::Path>) -> anyhow::Result<()> {
        let rust_gpu = self.get_rust_gpu_table();
        let build = rust_gpu.get_mut("build").unwrap().as_table_mut().unwrap();
        let output_dir = build.get_mut("output-dir").unwrap();
        *output_dir = toml::Value::String(format!("{}", path.as_ref().display()));
        self.write_shader_crate_cargo_toml_changes()?;
        Ok(())
    }

    /// Add or replace a dependency in the shader-crate-template
    fn set_dependency(&mut self, package: &str, version: &DependencyVersion) -> anyhow::Result<()> {
        let dependencies = self.get_cargo_dependencies_table();
        if let Some(value) = dependencies.get_mut(package) {
            version.modify_toml(value);
            self.write_shader_crate_cargo_toml_changes()?;
            Ok(())
        } else {
            anyhow::bail!("Crate `{package}` not found")
        }
    }

    /// Replace the `spirv-std` dependency version
    fn set_spirv_std_version(&mut self, version: &str) -> anyhow::Result<()> {
        self.set_dependency(
            "spirv-std",
            &DependencyVersion::parse(
                version.into(),
                Some("https://github.com/Rust-GPU/rust-gpu".into()),
            )?,
        )
    }

    /// Replace the `glam` dependency version
    fn set_dependency_glam(&mut self, version: &str) -> anyhow::Result<()> {
        self.set_dependency(
            "glam",
            &DependencyVersion::parse(
                version.into(),
                Some("https://github.com/bitshifter/glam-rs".into()),
            )?,
        )
    }
}

/// The version of a dependency
#[non_exhaustive]
pub enum DependencyVersion {
    /// Don't change anything, don't replace the dependency nor add it when it's not there.
    Latest,
    /// A version dependency for crates.io
    Crates(String),
    /// A git dependency on a specific rev
    Git {
        /// git repo
        git: String,
        /// git commit revision
        rev: String,
    },
}

impl DependencyVersion {
    /// Try to parse a version from a string
    ///
    /// # Errors
    /// if `version` is a commit rev, `git` must be specified
    pub fn parse(version: String, git: Option<String>) -> anyhow::Result<Self> {
        if version == "latest" {
            Ok(Self::Latest)
        } else if version.contains('.') {
            Ok(Self::Crates(version))
        } else {
            Ok(Self::Git {
                git: git.context("specifying a revision requires a git repo")?,
                rev: version,
            })
        }
    }

    /// Convert this version to a toml value, may fail if we want the latest version
    #[must_use]
    pub fn to_toml(&self) -> Option<toml::Table> {
        match self {
            Self::Latest => None,
            Self::Crates(version) => Some(toml::Table::from_iter([(
                "version".to_owned(),
                toml::Value::String(version.clone()),
            )])),
            Self::Git { git, rev } => Some(toml::Table::from_iter([
                ("git".to_owned(), toml::Value::String(git.clone())),
                ("rev".to_owned(), toml::Value::String(rev.clone())),
            ])),
        }
    }

    /// Convert this version to a toml value, may fail if we want the latest version
    pub fn modify_toml(&self, toml: &mut toml::Value) {
        if let Some(mut table) = self.to_toml() {
            let mut copy = |key: &str| {
                if let Some(src_table) = toml.as_table_mut()
                    && let Some(value) = src_table.remove(key)
                {
                    table.insert(key.to_owned(), value);
                }
            };
            copy("default-features");
            copy("features");
            *toml = toml::Value::Table(table);
        }
    }
}

/// Run the xtask.
fn main() -> anyhow::Result<()> {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .init();
    Cli::parse().run()
}

impl Cli {
    fn run(&self) -> anyhow::Result<()> {
        match &self {
            Cli::TestBuild {
                rust_gpu_version,
                glam_version,
            } => {
                log::info!("installing cargo gpu");
                cmd(["cargo", "install", "--path", "crates/cargo-gpu"])?;

                log::info!("setup project");
                let mut overwriter = ShaderCrateTemplateCargoTomlWriter::default();
                let dir = tempfile::TempDir::with_prefix("test-shader-output")?;
                overwriter.replace_output_dir(dir.path())?;
                if let Some(rust_gpu_version) = rust_gpu_version.as_ref() {
                    overwriter.set_spirv_std_version(rust_gpu_version)?;
                }
                if let Some(glam_version) = glam_version.as_ref() {
                    overwriter.set_dependency_glam(glam_version)?;
                }

                log::info!("building with auto-install");
                cmd([
                    "cargo",
                    "gpu",
                    "build",
                    "--shader-crate",
                    SHADER_CRATE_PATH,
                    "--auto-install-rust-toolchain",
                    "--force-overwrite-lockfiles-v4-to-v3",
                ])?;

                cmd(["ls", "-lah", dir.path().to_str().unwrap()])?;
                //NOTE: manifest.json is the default value here, which should be valid
                cmd(["cat", dir.path().join("manifest.json").to_str().unwrap()])?;
            }
            Cli::SetDependency {
                package,
                version,
                git,
            } => {
                let mut overwriter = ShaderCrateTemplateCargoTomlWriter::new(true);
                overwriter.set_dependency(
                    package,
                    &DependencyVersion::parse(version.clone(), git.clone())?,
                )?;
            }
            Cli::UpdateExpect => {
                let status = std::process::Command::new("cargo")
                    .args(["nextest", "run"])
                    .env("UPDATE_EXPECT", "1")
                    .status()?;
                anyhow::ensure!(status.success());
            }
            Cli::RustGpuRev { rev } => {
                let root = PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/../.."));
                let rev_regex = regex_lite::Regex::new(r#"rev\s*=\s*"[0-9a-f]*""#)?;
                let rev_replace = format!("rev = \"{rev}\"");
                let replace_rev = |file: &Path, dep: &str| -> anyhow::Result<()> {
                    log::info!("patching file `{}` dep `{dep}`", file.display());
                    let content = std::fs::read_to_string(file)?;
                    let content = content
                        // unlike `.lines()`, includes the `\n` or `\r\n` at the end of the line
                        .split_inclusive("\n")
                        .map(|line| {
                            if line.starts_with(dep) {
                                let replace = rev_regex.replace(line, &rev_replace);
                                assert!(
                                    matches!(replace, Cow::Owned(..)),
                                    "rev not found in line:\n{line}"
                                );
                                replace
                            } else {
                                Cow::Borrowed(line)
                            }
                        })
                        .collect::<String>();
                    std::fs::write(file, content.as_bytes())?;
                    Ok(())
                };
                replace_rev(&root.join("Cargo.toml"), "spirv-builder")?;
                replace_rev(
                    &root.join("crates/shader-crate-template/Cargo.toml"),
                    "spirv-std",
                )?;
                Cli::UpdateExpect.run()?;
            }
        }
        Ok(())
    }
}
