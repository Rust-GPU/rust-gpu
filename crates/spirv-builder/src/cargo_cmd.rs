use std::collections::HashSet;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::process::Command;

/// Filters the various env vars that a `cargo` child process would receive and reports back
/// what was inherited and what was removed. By default, removes all env vars that influences
/// the cargo invocations of a spirv-builder's build or cargo-gpu's install action.
pub struct CargoCmd {
    cargo: Command,
    vars_os: Vec<(OsString, OsString)>,
    removed: HashSet<OsString>,
}

impl CargoCmd {
    pub fn new() -> Self {
        let mut cargo = CargoCmd::new_no_filtering();

        // Clear Cargo environment variables that we don't want to leak into the
        // inner invocation of Cargo (because e.g. build scripts might read them),
        // before we set any of our own below.
        cargo.retain_vars_os(|(key, _)| {
            !key.to_str()
                .is_some_and(|s| s.starts_with("CARGO_FEATURES_") || s.starts_with("CARGO_CFG_"))
        });

        // NOTE(eddyb) Cargo caches some information it got from `rustc` in
        // `.rustc_info.json`, and assumes it only depends on the `rustc` binary,
        // but in our case, `rustc_codegen_spirv` changes are also relevant,
        // so we turn off that caching with an env var, just to avoid any issues.
        cargo.env("CARGO_CACHE_RUSTC_INFO", "0");

        // NOTE(firestar99) If you call SpirvBuilder in a build script, it will
        // set `RUSTC` before calling it. And if we were to propagate it to our
        // cargo invocation, it will take precedence over the `+toolchain` we
        // previously set.
        cargo.env_remove("RUSTC");

        // NOTE(tuguzT) Used by Cargo to call executables of Clippy, Miri
        // (and maybe other Cargo subcommands) instead of `rustc`
        // which could affect its functionality and break the build process.
        cargo.env_remove("RUSTC_WRAPPER");

        // NOTE(firestar99, tuguzT) Other environment variables that we don't want to
        // leak into the inner invocation of Cargo & break the build process.
        cargo
            .env_remove("RUSTC_WORKSPACE_WRAPPER")
            .env_remove("RUSTFLAGS")
            .env_remove("CARGO")
            .env_remove("RUSTUP_TOOLCHAIN");

        // NOTE(firestar99) Overwritten by spirv-builder anyway
        cargo.env_remove("CARGO_ENCODED_RUSTFLAGS");

        // NOTE(firestar99) Ignore any externally supplied target dir:
        // - spirv-builder: we overwrite it with `SpirvBuilder.target_dir_path` anyway
        // - cargo-gpu: we want to build it in our cache dir
        cargo.env_remove("CARGO_TARGET_DIR");

        cargo
    }

    pub fn new_no_filtering() -> Self {
        Self {
            cargo: Command::new("cargo"),
            vars_os: env::vars_os().collect(),
            removed: HashSet::default(),
        }
    }

    pub fn retain_vars_os(&mut self, mut f: impl FnMut((&OsString, &OsString)) -> bool) {
        for (key, value) in &self.vars_os {
            if !f((key, value)) {
                self.removed.insert(key.clone());
                self.cargo.env_remove(key);
            }
        }
    }

    pub fn env_remove(&mut self, key: impl AsRef<OsStr>) -> &mut Self {
        self.removed.insert(key.as_ref().to_os_string());
        self.cargo.env_remove(key);
        self
    }

    pub fn env(&mut self, key: impl AsRef<OsStr>, val: impl AsRef<OsStr>) -> &mut Self {
        self.removed.remove(key.as_ref());
        self.cargo.env(key, val);
        self
    }

    pub fn env_var_report(&self) -> EnvVarReport {
        let mut inherited = self.vars_os.clone();
        inherited.retain(|(key, _)| !self.removed.contains(key));
        EnvVarReport {
            inherited,
            removed: self.removed.clone(),
        }
    }
}

impl Default for CargoCmd {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for CargoCmd {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CargoCmd")
            .field("cargo", &self.cargo)
            .field("env_vars", &self.env_var_report())
            .finish()
    }
}

impl From<CargoCmd> for Command {
    fn from(cmd: CargoCmd) -> Self {
        cmd.cargo
    }
}

impl Deref for CargoCmd {
    type Target = Command;

    fn deref(&self) -> &Self::Target {
        &self.cargo
    }
}

impl DerefMut for CargoCmd {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cargo
    }
}

#[derive(Clone, Debug, Default)]
pub struct EnvVarReport {
    pub inherited: Vec<(OsString, OsString)>,
    pub removed: HashSet<OsString>,
}

impl Display for EnvVarReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let removed = self
            .removed
            .iter()
            .map(|key| format!("{}", key.to_string_lossy()))
            .collect::<Vec<_>>();
        let inherited = self
            .inherited
            .iter()
            .map(|(key, value)| format!("{}: {}", key.to_string_lossy(), value.to_string_lossy()))
            .collect::<Vec<_>>();

        f.debug_struct("EnvVarReport")
            .field("removed", &removed)
            .field("inherited", &inherited)
            .finish()
    }
}
