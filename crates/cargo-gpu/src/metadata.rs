//! Get config from the shader crate's `Cargo.toml` `[*.metadata.rust-gpu.*]`

use std::collections::HashMap;

use anyhow::Context as _;
use cargo_metadata::MetadataCommand;
use serde_json::Value;

/// A cache of metadata from various `Cargo.toml` files.
///
/// "Metadata" refers to the `[metadata.*]` section of `Cargo.toml` that `cargo` formally
/// ignores so that packages can implement their own behaviour with it.
#[derive(Debug, Default)]
pub struct MetadataCache {
    /// Cached result of `MetadataCommand::new().exec()`.
    inner: HashMap<std::path::PathBuf, cargo_metadata::Metadata>,
}

impl MetadataCache {
    /// Return the cached cargo metadata for the Cargo.toml at the given path,
    /// or find it, populate the cache with it and return it.
    fn get_metadata(
        &mut self,
        maybe_path_to_manifest_dir: Option<&std::path::Path>,
    ) -> anyhow::Result<&cargo_metadata::Metadata> {
        let path = if let Some(path) = maybe_path_to_manifest_dir {
            path.to_path_buf()
        } else {
            std::env::current_dir().context("cannot determine the current working directory")?
        };

        if !self.inner.contains_key(&path) {
            let metadata = MetadataCommand::new().current_dir(&path).exec()?;
            self.inner.insert(path.clone(), metadata);
        }

        self.inner.get(&path).context("unreachable")
    }

    /// Resolve a package name to a crate directory.
    ///
    /// ## Errors
    /// * if fetching cargo metadata fails.
    /// * if no packages are listed in the cargo metadata.
    /// * if the manifest path has no parent.
    pub fn resolve_package_to_shader_crate(
        &mut self,
        package: &str,
    ) -> anyhow::Result<std::path::PathBuf> {
        log::debug!("resolving package '{package}' to shader crate");
        let metadata = self.get_metadata(None)?;

        let meta_package = metadata
            .packages
            .iter()
            .find(|pkg| pkg.name.as_str() == package)
            .context("Package not found in metadata")?;
        let shader_crate_path: std::path::PathBuf = meta_package
            .manifest_path
            .parent()
            .context("manifest is missing a parent directory")?
            .to_path_buf()
            .into();
        log::debug!(
            "  determined shader crate path to be '{}'",
            shader_crate_path.display()
        );
        Ok(shader_crate_path)
    }

    /// Convert `rust-gpu`-specific sections in `Cargo.toml` to `clap`-compatible arguments.
    /// The section in question is: `[package.metadata.rust-gpu.*]`. See the `shader-crate-template`
    /// for an example.
    ///
    /// First we generate the CLI arg defaults as JSON. Then on top of those we merge any config
    /// from the workspace `Cargo.toml`, then on top of those we merge any config from the shader
    /// crate's `Cargo.toml`.
    ///
    /// ## Errors
    /// Errors if cargo metadata cannot be found or if it cannot be operated on.
    pub fn as_json(&mut self, path: &std::path::Path) -> anyhow::Result<Value> {
        log::debug!("reading package metadata from {}", path.display());
        let cargo_json = self.get_cargo_toml_as_json(path)?;
        let config = Self::merge_configs(&cargo_json, path)?;
        Ok(config)
    }

    /// Merge the various source of config: defaults, workspace and shader crate.
    fn merge_configs(
        cargo_json: &cargo_metadata::Metadata,
        path: &std::path::Path,
    ) -> anyhow::Result<Value> {
        log::debug!("merging cargo metadata from {}", path.display());
        let mut metadata = crate::config::Config::defaults_as_json()?;
        crate::config::Config::json_merge(
            &mut metadata,
            {
                log::debug!("looking for workspace metadata");
                let ws_meta = Self::get_rust_gpu_from_metadata(&cargo_json.workspace_metadata);
                log::trace!("workspace_metadata: {ws_meta:#?}");
                ws_meta
            },
            None,
        )?;
        crate::config::Config::json_merge(
            &mut metadata,
            {
                log::debug!("looking for crate metadata");
                let mut crate_meta = Self::get_crate_metadata(cargo_json, path)?;
                log::trace!("crate_metadata: {crate_meta:#?}");
                if let Some(output_path) = crate_meta.pointer_mut("/build/output_dir") {
                    log::debug!("found output-dir path in crate metadata: {output_path:?}");
                    if let Some(output_dir) = output_path.clone().as_str() {
                        let new_output_path = path.join(output_dir);
                        *output_path = Value::String(format!("{}", new_output_path.display()));
                        log::debug!(
                            "setting that to be relative to the Cargo.toml it was found in: {}",
                            new_output_path.display()
                        );
                    }
                }
                crate_meta
            },
            None,
        )?;

        Ok(metadata)
    }

    /// Convert a `Cargo.toml` to JSON
    fn get_cargo_toml_as_json(
        &mut self,
        path: &std::path::Path,
    ) -> anyhow::Result<cargo_metadata::Metadata> {
        self.get_metadata(Some(path)).cloned()
    }

    /// Get any `rust-gpu` metadata set in the crate's `Cargo.toml`
    fn get_crate_metadata(
        json: &cargo_metadata::Metadata,
        path: &std::path::Path,
    ) -> anyhow::Result<Value> {
        let shader_crate_path = std::fs::canonicalize(path)?.join("Cargo.toml");

        for package in &json.packages {
            let manifest_path = std::fs::canonicalize(package.manifest_path.as_std_path())?;
            log::debug!(
                "Matching shader crate path with manifest path: '{}' == '{}'?",
                shader_crate_path.display(),
                manifest_path.display()
            );
            if manifest_path == shader_crate_path {
                log::debug!("...matches! Getting metadata");
                return Ok(Self::get_rust_gpu_from_metadata(&package.metadata));
            }
        }
        Ok(serde_json::json!({}))
    }

    /// Get `rust-gpu` value from some metadata
    fn get_rust_gpu_from_metadata(metadata: &Value) -> Value {
        Self::keys_to_snake_case(
            metadata
                .pointer("/rust-gpu")
                .cloned()
                .unwrap_or(Value::Null),
        )
    }

    /// Convert JSON keys from kebab case to snake case. Eg: `a-b` to `a_b`.
    ///
    /// Detection of keys for serde deserialization must match the case in the Rust structs.
    /// However clap defaults to detecting CLI args in kebab case. So here we do the conversion.
    #[expect(clippy::wildcard_enum_match_arm, reason = "we only want objects")]
    fn keys_to_snake_case(json: Value) -> Value {
        match json {
            Value::Object(object) => Value::Object(
                object
                    .into_iter()
                    .map(|(key, value)| (key.replace('-', "_"), Self::keys_to_snake_case(value)))
                    .collect(),
            ),
            other => other,
        }
    }
}

#[expect(
    clippy::indexing_slicing,
    reason = "We don't need to be so strict in tests"
)]
#[cfg(test)]
mod test {
    use super::*;
    use std::path::Path;

    #[test_log::test]
    fn generates_defaults() {
        let mut metadata = MetadataCommand::new()
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .exec()
            .unwrap();
        metadata.packages.first_mut().unwrap().metadata = serde_json::json!({});
        let configs = MetadataCache::merge_configs(&metadata, Path::new("./")).unwrap();
        assert_eq!(configs["build"]["release"], Value::Bool(true));
        assert_eq!(
            configs["install"]["auto_install_rust_toolchain"],
            Value::Bool(false)
        );
    }

    #[test_log::test]
    fn can_override_config_from_workspace_toml() {
        let mut metadata = MetadataCommand::new()
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .exec()
            .unwrap();
        metadata.workspace_metadata = serde_json::json!({
            "rust-gpu": {
                "build": {
                    "release": false
                },
                "install": {
                    "auto-install-rust-toolchain": true
                }
            }
        });
        let configs = MetadataCache::merge_configs(&metadata, Path::new("./")).unwrap();
        assert_eq!(configs["build"]["release"], Value::Bool(false));
        assert_eq!(
            configs["install"]["auto_install_rust_toolchain"],
            Value::Bool(true)
        );
    }

    #[test_log::test]
    fn can_override_config_from_crate_toml() {
        let mut metadata = MetadataCommand::new()
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .exec()
            .unwrap();
        let cargo_gpu = metadata
            .packages
            .iter_mut()
            .find(|package| package.name.contains("cargo-gpu"))
            .unwrap();
        cargo_gpu.metadata = serde_json::json!({
            "rust-gpu": {
                "build": {
                    "release": false
                },
                "install": {
                    "auto-install-rust-toolchain": true
                }
            }
        });
        let configs = MetadataCache::merge_configs(&metadata, Path::new(".")).unwrap();
        assert_eq!(configs["build"]["release"], Value::Bool(false));
        assert_eq!(
            configs["install"]["auto_install_rust_toolchain"],
            Value::Bool(true)
        );
    }
}
