//! cargo-gpu build script.

use serde_json::Value;
use std::ffi::OsStr;
use std::path::PathBuf;

fn main() {
    compute_version_extra();
}

fn compute_version_extra() {
    let extra = version_crates_io()
        .or_else(version_git)
        .unwrap_or(String::from(" installed from unknown source"));

    std::fs::write(
        PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("version_extra"),
        extra,
    )
    .unwrap();
}

fn version_git() -> Option<String> {
    if let Some(git_directory) = invoke_git(["rev-parse", "--git-dir"]) {
        println!("cargo:rerun-if-changed={git_directory}/HEAD");
    }

    let git_rev = invoke_git(["rev-parse", "HEAD"]);
    let git_date = invoke_git([
        "show",
        "-s",
        "--format=%cd",
        "--date=format:%Y-%m-%d",
        "HEAD",
    ]);

    if let Some(rev) = git_rev
        && let Some(git_date) = git_date
    {
        let rev = rev.get(..8).unwrap_or(&rev);
        let git_date = git_date.strip_suffix("\n").unwrap_or(&git_date);
        Some(format!(" installed from repo at rev {rev} ({git_date})"))
    } else {
        None
    }
}

fn invoke_git<I, S>(args: I) -> Option<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    std::process::Command::new("git")
        .args(args)
        .output()
        .ok()
        .and_then(|output| String::from_utf8(output.stdout).ok())
}

/// `cargo package` adds the `.cargo_vcs_info.json` file on it's own, let's just use that to resolve the rev
fn version_crates_io() -> Option<String> {
    let json = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(".cargo_vcs_info.json"),
    )
    .ok()?;
    let value = serde_json::from_str::<Value>(&json).ok()?;
    let rev = value.get("git")?.get("sha1")?.as_str()?;
    let rev = rev.get(..8).unwrap_or(rev);
    Some(format!(" installed from crates.io (rev {rev})"))
}
