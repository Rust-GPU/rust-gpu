//! cargo-gpu build script.

fn main() {
    let git_hash = std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .map_or_else(
            |_| "unknown".to_owned(),
            |output| String::from_utf8(output.stdout).unwrap_or_else(|_| "unknown".to_owned()),
        );
    println!("cargo:rustc-env=GIT_HASH={git_hash}");
}
