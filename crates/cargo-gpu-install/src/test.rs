//! utilities for tests
#![cfg(any(feature = "test", test))]

use anyhow::Context;
use std::cell::RefCell;
use std::fs::File;
use std::io::Write as _;
use std::path::PathBuf;
use tempfile::TempDir;

/// `TestEnv` sets up a temp dir in `./target/cargo-gpu-test/` that is used as the cache dir. Not initializing a
/// `TestEnv` and asking for the cache dir will panic, to ensure you set it up. Calling [`Self::setup_shader_crate`]
/// or [`Self::setup_shader_crate_with_cargo_toml`] will copy the `shader_crate_template` into the temp dir and return
/// you the path to it, so each test now has it's unique copy and won't race to change the template in the repo.
/// Dropping `TestEnv` will clean up the dir, except when panic unwinding, so you can debug failures.
#[must_use]
pub struct TestEnv(TempDir);

impl TestEnv {
    /// Create a new [`TestEnv`]
    pub fn new() -> Self {
        let target_dir = cargo_metadata::MetadataCommand::new()
            .exec()
            .unwrap()
            .target_directory
            .into_std_path_buf();
        let tests_dir = target_dir.join("cargo-gpu-test");
        std::fs::create_dir_all(&tests_dir).ok();
        let test_dir = TempDir::new_in(tests_dir).unwrap();

        let had_old = TESTDIR
            .replace(Some(test_dir.path().to_path_buf()))
            .is_some();
        if had_old {
            panic!("TestEnv is not reentrant!")
        }

        TestEnv(test_dir)
    }

    /// Copies the `shader_crate_template` to the temp dir and returns the path to the directory.
    pub fn setup_shader_crate(&self) -> anyhow::Result<PathBuf> {
        let shader_crate_path = crate::cache_dir().unwrap().join("shader_crate");
        copy_dir_all(shader_crate_template_path(), &shader_crate_path)?;
        Ok(shader_crate_path)
    }

    /// Like [`Self::setup_shader_crate`], copies the `shader_crate_template` to the temp dir and returns the path to
    /// the directory. Additionally, takes a closure to allow you to overwrite the contents of the `Cargo.toml`.
    /// This function will write the bare minimum for a valid crate, that is, give it a package name.
    pub fn setup_shader_crate_with_cargo_toml(
        &self,
        f: impl FnOnce(&mut File) -> std::io::Result<()>,
    ) -> anyhow::Result<PathBuf> {
        let shader_crate_path = self.setup_shader_crate()?;
        let cargo_toml = shader_crate_path.join("Cargo.toml");
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .open(cargo_toml)?;
        writeln!(file, "[package]")?;
        writeln!(file, "name = \"test\"")?;
        f(&mut file)?;
        Ok(shader_crate_path)
    }
}

impl Drop for TestEnv {
    fn drop(&mut self) {
        TESTDIR.replace(None).unwrap();
        // when a test fails, keep directory
        if std::thread::panicking() {
            self.0.disable_cleanup(true);
        }
    }
}

thread_local! {
    static TESTDIR: RefCell<Option<PathBuf>> = RefCell::new(None);
}

/// [`crate::cache_dir`] for testing, see [`TestEnv`]
pub fn test_cache_dir() -> anyhow::Result<PathBuf> {
    Ok(TESTDIR.with_borrow(|a| a.clone()).context(
        "TestEnv is not initialized! Add `let _env = TestEnv::new();` to the beginning of your test",
    )?)
}

fn copy_dir_all(
    src: impl AsRef<std::path::Path>,
    dst: impl AsRef<std::path::Path>,
) -> anyhow::Result<()> {
    std::fs::create_dir_all(&dst)?;
    for maybe_entry in std::fs::read_dir(src)? {
        let entry = maybe_entry?;
        let ty = entry.file_type()?;
        if ty.is_dir() {
            copy_dir_all(entry.path(), dst.as_ref().join(entry.file_name()))?;
        } else {
            std::fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
        }
    }
    Ok(())
}

/// Path to the `shader-crate-template` for copying or querying data
pub fn shader_crate_template_path() -> PathBuf {
    let project_base = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    project_base.join("../shader-crate-template")
}
