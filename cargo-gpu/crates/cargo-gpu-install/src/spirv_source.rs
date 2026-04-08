//! Use the shader that we're compiling as the default source for which version of `rust-gpu` to use.
//!
//! We do this by calling `cargo tree` inside the shader's crate to get the defined `spirv-std`
//! version. Then with that we `git checkout` the `rust-gpu` repo that corresponds to that version.
//! From there we can look at the source code to get the required Rust toolchain.

use anyhow::Context as _;
use cargo_metadata::camino::{Utf8Path, Utf8PathBuf};
use cargo_metadata::semver::Version;
use cargo_metadata::{Metadata, MetadataCommand, Package};
use std::fs;
use std::path::{Path, PathBuf};

#[expect(
    clippy::doc_markdown,
    reason = "The URL should appear literally like this. But Clippy wants a markdown clickable link"
)]
/// The source and version of `rust-gpu`.
/// Eg:
///   * From crates.io with version "0.10.0"
///   * From Git with:
///     - a repo of "https://github.com/Rust-GPU/rust-gpu.git"
///     - a revision of "abc213"
///   * a local Path
#[non_exhaustive]
#[derive(Eq, PartialEq, Clone, Debug)]
pub enum SpirvSource {
    /// If the shader specifies a simple version like `spirv-std = "0.9.0"` then the source of
    /// `rust-gpu` is the conventional crates.io version.
    CratesIO(Version),
    /// If the shader specifies a version like:
    ///   `spirv-std = { git = "https://github.com..." ... }`
    /// then the source of `rust-gpu` is `Git`.
    Git {
        /// URL of the repository
        url: String,
        /// Revision or "commitsh"
        rev: String,
    },
    /// If the shader specifies a version like:
    ///   `spirv-std = { path = "/path/to/rust-gpu" ... }`
    /// then the source of `rust-gpu` is `Path`.
    Path {
        /// File path of rust-gpu repository
        rust_gpu_repo_root: Utf8PathBuf,
        /// Version of specified rust-gpu repository
        version: Version,
    },
}

impl core::fmt::Display for SpirvSource {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::CratesIO(version) => version.fmt(f),
            Self::Git { url, rev } => {
                // shorten rev to 8 chars, prevents windows compile errors due to too long paths... seriously
                if let Some(short_rev) = rev.get(..8) {
                    write!(f, "{url}+{short_rev}")
                } else {
                    write!(f, "{url}+{rev}")
                }
            }
            Self::Path {
                rust_gpu_repo_root,
                version,
            } => write!(f, "{rust_gpu_repo_root}+{version}"),
        }
    }
}

impl SpirvSource {
    /// Figures out which source of `rust-gpu` to use
    ///
    /// # Errors
    /// Crate may not depend on `spirv-std` or is otherwise malformed
    pub fn new(
        shader_crate_path: &Path,
        maybe_rust_gpu_source: Option<&str>,
        maybe_rust_gpu_version: Option<&str>,
    ) -> anyhow::Result<Self> {
        let source = if let Some(rust_gpu_version) = maybe_rust_gpu_version {
            if let Some(rust_gpu_source) = maybe_rust_gpu_source {
                Self::Git {
                    url: rust_gpu_source.to_owned(),
                    rev: rust_gpu_version.to_owned(),
                }
            } else {
                Self::CratesIO(Version::parse(rust_gpu_version)?)
            }
        } else {
            Self::get_rust_gpu_deps_from_shader(shader_crate_path).with_context(|| {
                format!(
                    "get spirv-std dependency from shader crate '{}'",
                    shader_crate_path.display()
                )
            })?
        };
        Ok(source)
    }

    /// Look into the shader crate to get the version of `rust-gpu` it's using.
    ///
    /// # Errors
    /// Crate may not depend on `spirv-std` or is otherwise malformed
    pub fn get_rust_gpu_deps_from_shader(shader_crate_path: &Path) -> anyhow::Result<Self> {
        let crate_metadata = query_metadata(shader_crate_path)?;
        let spirv_std_package = crate_metadata.find_package("spirv-std")?;
        let spirv_source = Self::parse_spirv_std_source_and_version(spirv_std_package)?;
        log::debug!(
            "Parsed `SpirvSource` from crate `{}`: \
            {spirv_source:?}",
            shader_crate_path.display(),
        );
        Ok(spirv_source)
    }

    /// Convert the `SpirvSource` to a cache directory in which we can build it.
    /// It needs to be dynamically created because an end-user might want to swap out the source,
    /// maybe using their own fork for example.
    ///
    /// # Errors
    /// [`crate::cache_dir`] may fail
    pub fn install_dir(&self) -> anyhow::Result<PathBuf> {
        match self {
            Self::Path {
                rust_gpu_repo_root, ..
            } => Ok(rust_gpu_repo_root.as_std_path().to_owned()),
            Self::CratesIO { .. } | Self::Git { .. } => {
                let dir = crate::to_dirname(self.to_string().as_ref());
                Ok(crate::cache_dir()?.join("codegen").join(dir))
            }
        }
    }

    /// Returns true if self is a Path
    #[must_use]
    pub const fn is_path(&self) -> bool {
        matches!(self, Self::Path { .. })
    }

    /// Parse a string like:
    ///   `spirv-std v0.9.0 (https://github.com/Rust-GPU/rust-gpu?rev=54f6978c#54f6978c) (*)`
    /// Which would return:
    ///   `SpirvSource::Git("https://github.com/Rust-GPU/rust-gpu", "54f6978c")`
    fn parse_spirv_std_source_and_version(spirv_std_package: &Package) -> anyhow::Result<Self> {
        log::trace!("parsing spirv-std source and version from package: '{spirv_std_package:?}'");

        let result = if let Some(source) = &spirv_std_package.source {
            let is_git = source.repr.starts_with("git+");
            let is_crates_io = source.is_crates_io();

            match (is_git, is_crates_io) {
                (true, true) => anyhow::bail!("parsed both git and crates.io?"),
                (true, false) => {
                    let parse_git = || {
                        let link = &source.repr.get(4..)?;
                        let sharp_index = link.find('#')?;
                        let url_end = link.find('?').unwrap_or(sharp_index);
                        let url = link.get(..url_end)?.to_owned();
                        let rev = link.get(sharp_index + 1..)?.to_owned();
                        Some(Self::Git { url, rev })
                    };
                    parse_git()
                        .with_context(|| format!("Failed to parse git url {}", &source.repr))?
                }
                (false, true) => Self::CratesIO(spirv_std_package.version.clone()),
                (false, false) => {
                    anyhow::bail!("Metadata of spirv-std package uses unknown url format!")
                }
            }
        } else {
            let rust_gpu_repo_root = spirv_std_package
                .manifest_path // rust-gpu/crates/spirv-std/Cargo.toml
                .parent() // rust-gpu/crates/spirv-std
                .and_then(Utf8Path::parent) // rust-gpu/crates
                .and_then(Utf8Path::parent) // rust-gpu
                .context("selecting rust-gpu workspace root dir in local path")?
                .to_owned();
            if !rust_gpu_repo_root.is_dir() {
                anyhow::bail!("path {rust_gpu_repo_root} is not a directory");
            }
            let version = spirv_std_package.version.clone();
            Self::Path {
                rust_gpu_repo_root,
                version,
            }
        };

        log::debug!("Parsed `rust-gpu` source and version: {result:?}");

        Ok(result)
    }
}

/// get the Package metadata from some crate
///
/// # Errors
/// metadata query may fail
pub fn query_metadata(crate_path: &Path) -> anyhow::Result<Metadata> {
    log::debug!("Running `cargo metadata` on `{}`", crate_path.display());
    let metadata = MetadataCommand::new()
        .current_dir(
            &crate_path
                .canonicalize()
                .context("could not get absolute path to shader crate")?,
        )
        .exec()?;
    Ok(metadata)
}

/// implements [`Self::find_package`]
pub trait FindPackage {
    /// Search for a package or return a nice error
    ///
    /// # Errors
    /// package may not be found or crate may be malformed
    fn find_package(&self, crate_name: &str) -> anyhow::Result<&Package>;
}

impl FindPackage for Metadata {
    fn find_package(&self, crate_name: &str) -> anyhow::Result<&Package> {
        if let Some(package) = self
            .packages
            .iter()
            .find(|package| package.name.as_str() == crate_name)
        {
            log::trace!("  found `{}` version `{}`", package.name, package.version);
            Ok(package)
        } else {
            anyhow::bail!(
                "`{crate_name}` not found in `Cargo.toml` at `{:?}`",
                self.workspace_root
            );
        }
    }
}

/// Parse the `rust-toolchain.toml` in the working tree of the checked-out version of the `rust-gpu` repo.
///
/// # Errors
/// parsing may fail
pub fn get_channel_from_rustc_codegen_spirv_build_script(
    rustc_codegen_spirv_package: &Package,
) -> anyhow::Result<String> {
    let path = rustc_codegen_spirv_package
        .manifest_path
        .parent()
        .context("finding `rustc_codegen_spirv` crate root")?;
    let build_rs = path.join("build.rs");

    log::debug!("Parsing `build.rs` at {build_rs:?} for the used toolchain");
    let contents = fs::read_to_string(&build_rs)?;
    let channel_start = "channel = \"";
    let channel_line = contents
        .lines()
        .find_map(|line| line.strip_prefix(channel_start))
        .context(format!("Can't find `{channel_start}` line in {build_rs:?}"))?;
    let channel = channel_line
        .get(..channel_line.find('"').context("ending \" missing")?)
        .context("can't slice version")?;
    Ok(channel.to_owned())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::TestEnv;
    use cargo_metadata::{PackageBuilder, PackageId, Source};
    use cargo_util_schemas::manifest::PackageName;
    use expect_test::expect;

    #[test_log::test]
    fn parsing_spirv_std_dep_for_shader_template() {
        let shader_template_path = crate::test::shader_crate_template_path();
        let source = SpirvSource::get_rust_gpu_deps_from_shader(&shader_template_path).unwrap();
        expect![[r#"
            Git {
                url: "https://github.com/Rust-GPU/rust-gpu",
                rev: "877bd8697a15f3e6d09446a5e1807e6237ca1dac",
            }"#]]
        .assert_eq(&format!("{source:#?}"));
    }

    #[test_log::test]
    fn cached_checkout_dir_sanity() {
        let _env = TestEnv::new();
        let shader_template_path = crate::test::shader_crate_template_path();
        let source = SpirvSource::get_rust_gpu_deps_from_shader(&shader_template_path).unwrap();
        let dir = source.install_dir().unwrap();
        let name = dir
            .file_name()
            .unwrap()
            .to_str()
            .map(std::string::ToString::to_string)
            .unwrap();
        expect!["https___github_com_Rust-GPU_rust-gpu+877bd869"].assert_eq(&name);
    }

    #[test_log::test]
    fn path_sanity() {
        let path = std::path::PathBuf::from("./");
        assert!(path.is_relative());
    }

    #[test_log::test]
    fn parse_git_with_rev() {
        let source = parse_git(
            "git+https://github.com/Rust-GPU/rust-gpu?rev=6a67e7b5954f37989ad540a555b5d6969073592e#86fc4803",
        );
        assert_eq!(
            source,
            SpirvSource::Git {
                url: "https://github.com/Rust-GPU/rust-gpu".to_owned(),
                rev: "86fc4803".to_owned(),
            }
        );
    }

    #[test_log::test]
    fn parse_git_no_question_mark() {
        // taken directly from Graphite
        let source = parse_git(
            "git+https://github.com/Rust-GPU/rust-gpu.git#6e2c84d4fe64e32df4c060c5a7f3e35a32e45421",
        );
        assert_eq!(
            source,
            SpirvSource::Git {
                url: "https://github.com/Rust-GPU/rust-gpu.git".to_owned(),
                rev: "6e2c84d4fe64e32df4c060c5a7f3e35a32e45421".to_owned(),
            }
        );
    }

    fn parse_git(source: &str) -> SpirvSource {
        let package = PackageBuilder::new(
            PackageName::new("spirv-std".to_owned()).unwrap(),
            Version::new(0, 9, 0),
            PackageId {
                repr: String::new(),
            },
            "",
        )
        .source(Some(Source {
            repr: source.to_owned(),
        }))
        .build()
        .unwrap();
        SpirvSource::parse_spirv_std_source_and_version(&package).unwrap()
    }
}
