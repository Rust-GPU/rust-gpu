use semver::Version;
use std::process::Command;

pub fn query_rustc_version(toolchain: Option<&str>) -> std::io::Result<Version> {
    let mut cmd = Command::new("rustc");
    if let Some(toolchain) = toolchain {
        cmd.arg(format!("+{toolchain}"));
    }
    cmd.arg("--version");
    let output = cmd.output()?;

    let stdout = String::from_utf8(output.stdout).expect("stdout must be utf-8");
    let parse = |output: &str| {
        let output = output.strip_prefix("rustc ")?;
        let version = &output[..output.find(|c| !"0123456789.".contains(c))?];
        Version::parse(version).ok()
    };
    Ok(parse(&stdout)
        .unwrap_or_else(|| panic!("failed parsing `rustc --version` output `{stdout}`")))
}
