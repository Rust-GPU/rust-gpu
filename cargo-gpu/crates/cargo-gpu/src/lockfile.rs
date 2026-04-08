//! Handles lockfile version conflicts and downgrades. Stable uses lockfile v4, but rust-gpu
//! v0.9.0 uses an old toolchain requiring v3 and will refuse to build with a v4 lockfile being
//! present. This module takes care of warning the user and potentially downgrading the lockfile.

use crate::spirv_builder::query_rustc_version;
use anyhow::Context as _;
use semver::Version;
use std::io::Write as _;

/// `Cargo.lock` manifest version 4 became the default in Rust 1.83.0. Conflicting manifest
/// versions between the workspace and the shader crate, can cause problems.
const RUST_VERSION_THAT_USES_V4_CARGO_LOCKS: Version = Version::new(1, 83, 0);

/// Cargo dependency for `spirv-builder` and the rust toolchain channel.
#[derive(Debug, Clone)]
pub struct LockfileMismatchHandler {
    /// `Cargo.lock`s that have had their manifest versions changed by us and need changing back.
    pub cargo_lock_files_with_changed_manifest_versions: Vec<std::path::PathBuf>,
}

impl LockfileMismatchHandler {
    /// Create instance
    pub fn new(
        shader_crate_path: &std::path::Path,
        toolchain_channel: &str,
        is_force_overwrite_lockfiles_v4_to_v3: bool,
    ) -> anyhow::Result<Self> {
        let mut cargo_lock_files_with_changed_manifest_versions = vec![];

        let maybe_shader_crate_lock =
            Self::ensure_workspace_rust_version_doesnt_conflict_with_shader(
                shader_crate_path,
                is_force_overwrite_lockfiles_v4_to_v3,
            )
            .context("ensure_workspace_rust_version_doesnt_conflict_with_shader")?;

        if let Some(shader_crate_lock) = maybe_shader_crate_lock {
            cargo_lock_files_with_changed_manifest_versions.push(shader_crate_lock);
        }

        let maybe_workspace_crate_lock =
            Self::ensure_shader_rust_version_doesnt_conflict_with_any_cargo_locks(
                shader_crate_path,
                toolchain_channel,
                is_force_overwrite_lockfiles_v4_to_v3,
            )
            .context("ensure_shader_rust_version_doesnt_conflict_with_any_cargo_locks")?;

        if let Some(workspace_crate_lock) = maybe_workspace_crate_lock {
            cargo_lock_files_with_changed_manifest_versions.push(workspace_crate_lock);
        }

        Ok(Self {
            cargo_lock_files_with_changed_manifest_versions,
        })
    }

    /// See docs for `force_overwrite_lockfiles_v4_to_v3` flag for why we do this.
    fn ensure_workspace_rust_version_doesnt_conflict_with_shader(
        shader_crate_path: &std::path::Path,
        is_force_overwrite_lockfiles_v4_to_v3: bool,
    ) -> anyhow::Result<Option<std::path::PathBuf>> {
        log::debug!("Ensuring no v3/v4 `Cargo.lock` conflicts from workspace Rust...");
        let workspace_rust_version = query_rustc_version(None).context("reading rustc version")?;
        if workspace_rust_version >= RUST_VERSION_THAT_USES_V4_CARGO_LOCKS {
            log::debug!(
                "user's Rust is v{workspace_rust_version}, so no v3/v4 conflicts possible."
            );
            return Ok(None);
        }

        Self::handle_conflicting_cargo_lock_v4(
            shader_crate_path,
            is_force_overwrite_lockfiles_v4_to_v3,
        )
        .context("handling v4/v3 conflict")?;

        if is_force_overwrite_lockfiles_v4_to_v3 {
            Ok(Some(shader_crate_path.join("Cargo.lock")))
        } else {
            Ok(None)
        }
    }

    /// See docs for `force_overwrite_lockfiles_v4_to_v3` flag for why we do this.
    fn ensure_shader_rust_version_doesnt_conflict_with_any_cargo_locks(
        shader_crate_path: &std::path::Path,
        channel: &str,
        is_force_overwrite_lockfiles_v4_to_v3: bool,
    ) -> anyhow::Result<Option<std::path::PathBuf>> {
        log::debug!("Ensuring no v3/v4 `Cargo.lock` conflicts from shader's Rust...");
        let shader_rust_version =
            query_rustc_version(Some(channel)).context("getting rustc version")?;
        if shader_rust_version >= RUST_VERSION_THAT_USES_V4_CARGO_LOCKS {
            log::debug!("shader's Rust is v{shader_rust_version}, so no v3/v4 conflicts possible.");
            return Ok(None);
        }

        log::debug!(
            "shader's Rust is v{shader_rust_version}, so checking both shader and workspace `Cargo.lock` manifest versions..."
        );

        if shader_crate_path.join("Cargo.lock").exists() {
            // Note that we don't return the `Cargo.lock` here (so that it's marked for reversion
            // after the build), because we can be sure that updating it now is actually updating it
            // to the state it should have been all along. Therefore it doesn't need reverting once
            // fixed.
            Self::handle_conflicting_cargo_lock_v4(
                shader_crate_path,
                is_force_overwrite_lockfiles_v4_to_v3,
            )
            .context("handling v4/v3 conflict")?;
        }

        if let Some(workspace_root) =
            Self::get_workspace_root(shader_crate_path).context("reading workspace root")?
        {
            Self::handle_conflicting_cargo_lock_v4(
                workspace_root,
                is_force_overwrite_lockfiles_v4_to_v3,
            )
            .context("handling conflicting cargo v4")?;
            return Ok(Some(workspace_root.join("Cargo.lock")));
        }

        Ok(None)
    }

    /// Get the path to the shader crate's workspace, if it has one. We can't use the traditional
    /// `cargo metadata` because if the workspace has a conflicting `Cargo.lock` manifest version
    /// then that command won't work. Instead we do an old school recursive file tree walk.
    fn get_workspace_root(
        shader_crate_path: &std::path::Path,
    ) -> anyhow::Result<Option<&std::path::Path>> {
        let shader_cargo_toml = std::fs::read_to_string(shader_crate_path.join("Cargo.toml"))
            .with_context(|| format!("reading Cargo.toml at {}", shader_crate_path.display()))?;
        if !shader_cargo_toml.contains("workspace = true") {
            return Ok(None);
        }

        let mut current_path = shader_crate_path;
        #[expect(clippy::default_numeric_fallback, reason = "It's just a loop")]
        for _ in 0..15 {
            if let Some(parent_path) = current_path.parent() {
                if parent_path.join("Cargo.lock").exists() {
                    return Ok(Some(parent_path));
                }
                current_path = parent_path;
            } else {
                break;
            }
        }

        Ok(None)
    }

    /// When Rust < 1.83.0 is being used an error will occur if it tries to parse `Cargo.lock`
    /// files that use lockfile manifest version 4. Here we check and handle that.
    fn handle_conflicting_cargo_lock_v4(
        folder: &std::path::Path,
        is_force_overwrite_lockfiles_v4_to_v3: bool,
    ) -> anyhow::Result<()> {
        let shader_cargo_lock_path = folder.join("Cargo.lock");
        let shader_cargo_lock = std::fs::read_to_string(shader_cargo_lock_path.clone())
            .context("reading shader cargo lock")?;
        let third_line = shader_cargo_lock.lines().nth(2).context("no third line")?;
        if third_line.contains("version = 4") {
            Self::handle_v3v4_conflict(
                &shader_cargo_lock_path,
                is_force_overwrite_lockfiles_v4_to_v3,
            )
            .context("handling v4/v3 conflict")?;
            return Ok(());
        }
        if third_line.contains("version = 3") {
            return Ok(());
        }
        anyhow::bail!(
            "Unrecognized `Cargo.lock` manifest version at: {}",
            folder.display()
        )
    }

    /// Handle conflicting `Cargo.lock` manifest versions by either overwriting the manifest
    /// version or exiting with advice on how to handle the conflict.
    fn handle_v3v4_conflict(
        offending_cargo_lock: &std::path::Path,
        is_force_overwrite_lockfiles_v4_to_v3: bool,
    ) -> anyhow::Result<()> {
        if !is_force_overwrite_lockfiles_v4_to_v3 {
            Self::exit_with_v3v4_hack_suggestion();
        }

        Self::replace_cargo_lock_manifest_version(offending_cargo_lock, "4", "3")
            .context("replacing version 4 -> 3")?;

        Ok(())
    }

    /// Once all install and builds have completed put their manifest versions back to how they
    /// were.
    pub fn revert_cargo_lock_manifest_versions(&self) -> anyhow::Result<()> {
        for offending_cargo_lock in &self.cargo_lock_files_with_changed_manifest_versions {
            log::debug!("Reverting: {}", offending_cargo_lock.display());
            Self::replace_cargo_lock_manifest_version(offending_cargo_lock, "3", "4")
                .context("replacing version 3 -> 4")?;
        }

        Ok(())
    }

    /// Replace the manifest version, eg `version = 4`, in a `Cargo.lock` file.
    fn replace_cargo_lock_manifest_version(
        offending_cargo_lock: &std::path::Path,
        from_version: &str,
        to_version: &str,
    ) -> anyhow::Result<()> {
        log::warn!(
            "Replacing manifest version 'version = {}' with 'version = {}' in: {}",
            from_version,
            to_version,
            offending_cargo_lock.display()
        );
        let old_contents = std::fs::read_to_string(offending_cargo_lock)
            .context("reading offending Cargo.lock")?;
        let new_contents = old_contents.replace(
            &format!("\nversion = {from_version}\n"),
            &format!("\nversion = {to_version}\n"),
        );

        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .open(offending_cargo_lock)
            .context("opening offending Cargo.lock")?;
        file.write_all(new_contents.as_bytes())?;

        Ok(())
    }

    /// Exit and give the user advice on how to deal with the infamous v3/v4 Cargo lockfile version
    /// problem.
    #[expect(clippy::non_ascii_literal, reason = "It's CLI output")]
    fn exit_with_v3v4_hack_suggestion() {
        crate::user_output!(
            "Conflicting `Cargo.lock` versions detected ⚠️\n\
            Because `cargo gpu` uses a dedicated Rust toolchain for compiling shaders\n\
            it's possible that the `Cargo.lock` manifest version of the shader crate\n\
            does not match the `Cargo.lock` manifest version of the workspace. This is\n\
            due to a change in the defaults introduced in Rust 1.83.0.\n\
            \n\
            One way to resolve this is to force the workspace to use the same version\n\
            of Rust as required by the shader. However that is not often ideal or even\n\
            possible. Another way is to exlude the shader from the workspace. This is\n\
            also not ideal if you have many shaders sharing config from the workspace.\n\
            \n\
            Therefore `cargo gpu build/install` offers a workaround with the argument:\n\
              --force-overwrite-lockfiles-v4-to-v3\n\
            \n\
            See `cargo gpu build --help` for more information.\n\
            "
        );
        std::process::exit(1);
    }
}

impl Drop for LockfileMismatchHandler {
    fn drop(&mut self) {
        let result = self.revert_cargo_lock_manifest_versions();
        if let Err(error) = result {
            log::error!("Couldn't revert some or all of the shader `Cargo.lock` files: {error}");
        }
    }
}
