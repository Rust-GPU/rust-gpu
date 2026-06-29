//! cargo-gpu build script.

use std::ffi::OsStr;

fn main() {
    let git_directory = invoke_git(["rev-parse", "--git-dir"]);
    println!("cargo:rerun-if-changed={git_directory}/HEAD");
    println!("cargo:rerun-if-changed={git_directory}/packed-refs");
    if let Ok(git_head) = std::fs::read_to_string(format!("{git_directory}/HEAD"))
        && let Some(git_ref) = git_head.strip_prefix("ref: ")
    {
        println!("cargo:rerun-if-changed={git_directory}/{}", git_ref.trim());
    }

    let git_hash = invoke_git(["rev-parse", "HEAD"]);
    println!("cargo:rustc-env=GIT_HASH={git_hash}");

    let git_date = invoke_git([
        "show",
        "-s",
        "--format=%cd",
        "--date=format:%Y-%m-%d",
        "HEAD",
    ]);
    println!("cargo:rustc-env=GIT_DATE={git_date}");
}

fn invoke_git<I, S>(args: I) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    std::process::Command::new("git")
        .args(args)
        .output()
        .ok()
        .and_then(|output| String::from_utf8(output.stdout).ok())
        .unwrap_or_else(|| "unknown".to_owned())
}
