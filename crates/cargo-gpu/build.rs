//! cargo-gpu build script.

use std::ffi::OsStr;
use std::path::PathBuf;

fn main() {
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

    let extra = if let Some(git_rev) = git_rev
        && let Some(git_date) = git_date
    {
        let git_rev = git_rev.get(..8).unwrap_or(&git_rev);
        let git_date = git_date.strip_suffix("\n").unwrap_or(&git_date);
        format!(" ({git_rev} {git_date})")
    } else {
        " (installed from crates.io)".into()
    };
    std::fs::write(
        PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("version_extra"),
        extra,
    )
    .unwrap();
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
