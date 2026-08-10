use anyhow::Context;
use chrono::{Datelike, Utc};
use std::env;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// Set the toolchain of rust-gpu to some nightly
#[derive(Debug, clap::Parser)]
pub struct SetToolchain {
    /// nightly version to set the toolchain to
    ///
    /// accepts `nightly-1234-56-78` or `1234-56-78`, defaults to today's nightly
    version: Option<String>,
}

impl SetToolchain {
    pub fn run(&self) -> anyhow::Result<()> {
        // figure out version and commit-hash
        let version = self.version.as_ref().map_or_else(
            || {
                let time = Utc::now();
                format!(
                    "nightly-{:04}-{:02}-{:02}",
                    time.year(),
                    time.month(),
                    time.day()
                )
            },
            |version| {
                if version.starts_with("nightly-") {
                    version.clone()
                } else {
                    format!("nightly-{version}")
                }
            },
        );
        println!("Updating toolchain to {version}");
        let commit_hash = get_rustc_commit_hash(&version)?;

        // update files
        let regex_channel = regex_lite::Regex::new(r#"channel = "[a-zA-Z\-0-9]*""#)?;
        let regex_commit_hash = regex_lite::Regex::new(r#"commit_hash = [0-9a-f]*"#)?;
        let update_file = |file: &Path| -> anyhow::Result<()> {
            let content = std::fs::read_to_string(file)?;
            let content = regex_channel.replace(&content, format!(r#"channel = "{version}""#));
            let content =
                regex_commit_hash.replace(&content, format!(r#"commit_hash = {commit_hash}"#));
            std::fs::write(file, &*content)?;
            Ok(())
        };
        let root = PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/../.."));
        update_file(&root.join("rust-toolchain.toml"))?;
        update_file(&root.join("crates/rustc_codegen_spirv/build.rs"))?;

        // if jj is available and desc is empty, update desc
        if let Ok(output) = Command::new("jj")
            .args(["log", "-Gr", "@", "-T", "description"])
            .output()
            && String::from_utf8(output.stdout)?.trim().is_empty()
        {
            Command::new("jj")
                .args(["desc", "-m"])
                .arg(format!("update to {version}"))
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
                .ok();
        }
        Ok(())
    }
}

fn get_rustc_commit_hash(toolchain: &str) -> anyhow::Result<String> {
    let stdout = String::from_utf8(
        Command::new("rustc")
            .arg(format!("+{toolchain}"))
            .arg("-vV")
            .stderr(Stdio::inherit())
            .output()?
            .stdout,
    )?;
    stdout
        .lines()
        .find_map(|l| l.strip_prefix("commit-hash: "))
        .map(ToString::to_string)
        .context("`commit-hash` not found in `rustc -vV` output")
}
