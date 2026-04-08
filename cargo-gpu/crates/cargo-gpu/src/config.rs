//! Manage and merge the various sources of config: shader crate's `Cargo.toml`(s) and CLI args.
use anyhow::Context as _;
use clap::Parser as _;

use crate::metadata::MetadataCache;

/// Config
pub struct Config;

impl Config {
    /// Get all the config defaults as JSON.
    pub fn defaults_as_json() -> anyhow::Result<serde_json::Value> {
        let defaults_json = Self::cli_args_to_json(vec![String::new()])?;
        Ok(defaults_json)
    }

    /// Convert CLI args to their serde JSON representation.
    fn cli_args_to_json(env_args: Vec<String>) -> anyhow::Result<serde_json::Value> {
        Ok(serde_json::to_value(crate::build::Build::parse_from(
            env_args,
        ))?)
    }

    /// Config for the `cargo gpu build` and `cargo gpu install` can be set in the shader crate's
    /// `Cargo.toml`, so here we load that config first as the base config, and the CLI arguments can
    /// then later override it.
    pub fn clap_command_with_cargo_config(
        shader_crate_path: &std::path::Path,
        mut env_args: Vec<String>,
        metadata: &mut MetadataCache,
    ) -> anyhow::Result<crate::build::Build> {
        let mut config = metadata.as_json(shader_crate_path)?;

        env_args.retain(|arg| {
            !(arg == "build" || arg == "install" || arg == "check" || arg == "clippy")
        });
        let cli_args_json = Self::cli_args_to_json(env_args)?;
        Self::json_merge(&mut config, cli_args_json, None)?;

        let args = serde_json::from_value::<crate::build::Build>(config)?;
        Ok(args)
    }

    /// Merge 2 JSON objects. But only if the incoming patch value isn't the default value.
    /// Inspired by: <https://stackoverflow.com/a/47142105/575773>
    pub fn json_merge(
        left_in: &mut serde_json::Value,
        right_in: serde_json::Value,
        maybe_pointer: Option<&String>,
    ) -> anyhow::Result<()> {
        let defaults = Self::defaults_as_json()?;

        match (left_in, right_in) {
            (left @ &mut serde_json::Value::Object(_), serde_json::Value::Object(right)) => {
                let left_as_object = left
                    .as_object_mut()
                    .context("Unreachable, we've already proved it's an object")?;
                for (key, value) in right {
                    let new_pointer = maybe_pointer.as_ref().map_or_else(
                        || format!("/{}", key.clone()),
                        |pointer| format!("{}/{}", pointer, key.clone()),
                    );
                    Self::json_merge(
                        left_as_object
                            .entry(key.clone())
                            .or_insert(serde_json::Value::Null),
                        value,
                        Some(&new_pointer),
                    )?;
                }
            }
            (left, right) => {
                if let Some(pointer) = maybe_pointer {
                    let default = defaults.pointer(pointer).context(format!(
                        "Configuration option with path `{pointer}` was not found in the default configuration, \
                        which is:\ndefaults: {defaults:#?}"
                    ))?;
                    if &right != default {
                        // Only overwrite if the new value differs from the defaults.
                        *left = right;
                    }
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::spirv_builder::Capability;
    use crate::test::TestEnv;
    use std::io::Write as _;

    #[test_log::test]
    fn booleans_from_cli() {
        let _env = TestEnv::new();
        let shader_crate_path = _env.setup_shader_crate().unwrap();

        let args = Config::clap_command_with_cargo_config(
            &shader_crate_path,
            vec![
                "gpu".to_owned(),
                "build".to_owned(),
                "--debug".to_owned(),
                "--auto-install-rust-toolchain".to_owned(),
            ],
            &mut MetadataCache::default(),
        )
        .unwrap();
        assert!(!args.build.spirv_builder.release);
        assert!(args.install.auto_install_rust_toolchain);
    }

    #[test_log::test]
    fn booleans_from_cargo() {
        let _env = TestEnv::new();
        let shader_crate_path = _env
            .setup_shader_crate_with_cargo_toml(|file| {
                file.write_all(
                    [
                        "[package.metadata.rust-gpu.build]",
                        "release = false",
                        "[package.metadata.rust-gpu.install]",
                        "auto-install-rust-toolchain = true",
                    ]
                    .join("\n")
                    .as_bytes(),
                )
            })
            .unwrap();

        let args = Config::clap_command_with_cargo_config(
            &shader_crate_path,
            vec![],
            &mut MetadataCache::default(),
        )
        .unwrap();
        assert!(!args.build.spirv_builder.release);
        assert!(args.install.auto_install_rust_toolchain);
    }

    fn update_cargo_output_dir(_env: &TestEnv) -> std::path::PathBuf {
        _env.setup_shader_crate_with_cargo_toml(|file| {
            file.write_all(
                [
                    "[package.metadata.rust-gpu.build]",
                    "output-dir = \"/the/moon\"",
                ]
                .join("\n")
                .as_bytes(),
            )
        })
        .unwrap()
    }

    #[test_log::test]
    fn string_from_cargo() {
        let _env = TestEnv::new();
        let shader_crate_path = update_cargo_output_dir(&_env);

        let args = Config::clap_command_with_cargo_config(
            &shader_crate_path,
            vec![],
            &mut MetadataCache::default(),
        )
        .unwrap();
        if cfg!(target_os = "windows") {
            assert!(
                args.build
                    .output_dir
                    .to_str()
                    .unwrap()
                    .ends_with("/the/moon"),
                "Actual: {:?}",
                args.build.output_dir
            );
        } else {
            assert_eq!(args.build.output_dir, std::path::Path::new("/the/moon"));
        }
    }

    #[test_log::test]
    fn string_from_cargo_overwritten_by_cli() {
        let _env = TestEnv::new();
        let shader_crate_path = update_cargo_output_dir(&_env);

        let args = Config::clap_command_with_cargo_config(
            &shader_crate_path,
            vec![
                "gpu".to_owned(),
                "build".to_owned(),
                "--output-dir".to_owned(),
                "/the/river".to_owned(),
            ],
            &mut MetadataCache::default(),
        )
        .unwrap();
        assert_eq!(args.build.output_dir, std::path::Path::new("/the/river"));
    }

    #[test_log::test]
    fn arrays_from_cargo() {
        let _env = TestEnv::new();
        let shader_crate_path = _env
            .setup_shader_crate_with_cargo_toml(|file| {
                file.write_all(
                    [
                        "[package.metadata.rust-gpu.build]",
                        "capabilities = [\"AtomicStorage\", \"Matrix\"]",
                    ]
                    .join("\n")
                    .as_bytes(),
                )
            })
            .unwrap();

        let args = Config::clap_command_with_cargo_config(
            &shader_crate_path,
            vec![],
            &mut MetadataCache::default(),
        )
        .unwrap();
        assert_eq!(
            args.build.spirv_builder.capabilities,
            vec![Capability::AtomicStorage, Capability::Matrix]
        );
    }

    #[test_log::test]
    fn rename_manifest_parse() {
        let _env = TestEnv::new();
        let shader_crate_path = _env.setup_shader_crate().unwrap();

        let args = Config::clap_command_with_cargo_config(
            &shader_crate_path,
            vec![
                "gpu".to_owned(),
                "build".to_owned(),
                "--manifest-file".to_owned(),
                "mymanifest".to_owned(),
            ],
            &mut MetadataCache::default(),
        )
        .unwrap();
        assert_eq!(args.build.manifest_file, "mymanifest".to_owned());
    }
}
