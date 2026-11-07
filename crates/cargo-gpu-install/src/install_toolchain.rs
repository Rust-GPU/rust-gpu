//! toolchain installation logic

use crate::user_output;
use anyhow::Context as _;
#[cfg(feature = "tty")]
use crossterm::tty::IsTty as _;
use std::collections::HashSet;
use std::process::Command;
use std::string::FromUtf8Error;

/// list of required rustup components
pub const REQUIRED_COMPONENTS: &[&str] = [
    "cargo",
    "rustc",
    "rust-std",
    "clippy",
    "rust-src",
    "rustc-dev",
    "llvm-tools",
]
.as_slice();

/// Use `rustup` to install the toolchain and components, if not already installed.
///
/// Pretty much runs:
///
/// * rustup toolchain add nightly-2024-04-24
/// * rustup component add --toolchain nightly-2024-04-24 rust-src rustc-dev llvm-tools
pub fn ensure_toolchain_and_components_exist(
    channel: &str,
    skip_toolchain_install_consent: bool,
) -> anyhow::Result<()> {
    // While our channel may be `nightly-2024-04-24`, it'll be resolved to the full toolchain name of e.g.
    // `nightly-2024-04-24-aarch64-unknown-linux-gnu` and that's also what `rustup toolchain list` will print.
    // Only checking whether the toolchain starts with the channel name may incorrectly pass if you have a toolchain
    // installed that you're not able to run on your system via `rustup toolchain install --force-non-host ...`.
    // CMD: `rustc --print host-tuple`
    // TODO: What if the user has no toolchain installed? You can't query this with rustup sady.
    let (host_tuple, _) = run_cmd(Command::new("rustc").args(["--print", "host-tuple"]))?;
    let host_tuple = host_tuple.trim_ascii();
    let toolchain = format!("{channel}-{host_tuple}");

    // Two concurrent `cargo-gpu` processes (e.g. build scripts of multiple shader crates running
    // in parallel) can otherwise observe a toolchain that another process is still in the middle
    // of installing: `rustup toolchain list` already lists it, but its manifest isn't written yet,
    // and the `component list` below fails with `error: Missing manifest in toolchain '...'`.
    //
    // So hold a lock across the entire check-and-install. It is taken unconditionally, even when
    // the codegen backend is already built and the `install_dir` lock in `install.rs` is never
    // taken at all, since we always query `rustup` here.
    let _rustup_lock = crate::install::FileLock::lock_with_message(&rustup_lock_path()?, "rustup")
        .context("acquiring rustup file lock")?;

    if !is_toolchain_installed(&toolchain, host_tuple)? {
        let message = format!(
            "toolchain {channel} with components {}",
            intersperse(", ", REQUIRED_COMPONENTS.iter().copied())
        );
        get_consent_for_toolchain_install(
            format!("Install {message}").as_ref(),
            skip_toolchain_install_consent,
        )?;
        user_output!("Installing {message}\n");

        // component list may be out of sync
        // CMD: `rustup toolchain install nightly-2024-04-24 -c clippy,rust-src,rustc-dev,llvm-tools`
        run_cmd(
            Command::new("rustup")
                .args([
                    "toolchain",
                    "install",
                    &toolchain,
                    "--profile",
                    "minimal",
                    "-c",
                    &intersperse(",", REQUIRED_COMPONENTS.iter().copied()),
                ])
                .stdout(std::process::Stdio::inherit())
                .stderr(std::process::Stdio::inherit()),
        )?;
    }

    Ok(())
}

/// Path of the file lock guarding our `rustup` invocations.
fn rustup_lock_path() -> anyhow::Result<std::path::PathBuf> {
    let (home, _) = run_cmd(Command::new("rustup").args(["show", "home"]))?;
    // FNV-1a, needs to be stable across Rust versions
    let hash = home
        .trim_ascii()
        .bytes()
        .fold(0xcbf2_9ce4_8422_2325_u64, |hash, byte| {
            (hash ^ u64::from(byte)).wrapping_mul(0x0000_0100_0000_01b3)
        });
    Ok(std::env::temp_dir().join(format!("rust-gpu-rustup-{hash:016x}.lock")))
}

/// Returns true if the toolchain and required components are installed.
fn is_toolchain_installed(toolchain: &str, host_tuple: &str) -> anyhow::Result<bool> {
    // check if toolchain is installed
    // CMD: `rustup toolchain list -q`
    let (list_toolchains, _) = run_cmd(Command::new("rustup").args(["toolchain", "list", "-q"]))?;
    if !is_toolchain_listed(&list_toolchains, toolchain) {
        log::info!("toolchain {toolchain} is not installed");
        return Ok(false);
    }

    // check if required components are installed
    // NOTE: checking for components will install the toolchain with the default profile, if not already installed!
    // So we must check beforehand whether the toolchain is installed, to not accidentally install it here.
    // Passing *just* `-q` will list available components, so add `--installed` for installed components.
    // CMD: `rustup component list --toolchain nightly-2024-04-24-aarch64-unknown-linux-gnu -q --installed`
    let (components, _) = run_cmd(Command::new("rustup").args([
        "component",
        "list",
        "--toolchain",
        toolchain,
        "-q",
        "--installed",
    ]))?;

    let missing = missing_components(&components, host_tuple);
    if !missing.is_empty() {
        log::info!("components {missing:?} missing for toolchain {toolchain}");
        return Ok(false);
    }

    log::info!("toolchain and required components are already installed");
    Ok(true)
}

/// Whether `toolchain` is in the output of `rustup toolchain list -q`.
///
/// Entries may carry annotations like `(active, default)`, so only exact words match.
fn is_toolchain_listed(list_output: &str, toolchain: &str) -> bool {
    list_output.split_ascii_whitespace().any(|s| s == toolchain)
}

/// The [`REQUIRED_COMPONENTS`] absent from the output of `rustup component list -q --installed`.
///
/// Components are listed either as:
/// * `llvm-tools-aarch64-unknown-linux-gnu`, where we strip the host tuple from the end
/// * `rust-src`, since source code isn't target dependent
fn missing_components(list_output: &str, host_tuple: &str) -> HashSet<&'static str> {
    let component_host_suffix = format!("-{host_tuple}");
    let mut missing = REQUIRED_COMPONENTS.iter().copied().collect::<HashSet<_>>();
    for component in list_output.split_ascii_whitespace() {
        missing.remove(
            component
                .strip_suffix(&component_host_suffix)
                .unwrap_or(component),
        );
    }
    missing
}

pub fn run_cmd(cmd: &mut Command) -> anyhow::Result<(String, String)> {
    let output = cmd.output();
    let fmt_cmd = || {
        intersperse(
            " ",
            std::iter::once(cmd.get_program())
                .chain(cmd.get_args())
                .map(|s| s.to_string_lossy()),
        )
    };
    let output = output.with_context(|| format!("Failed to launch cmd `{}`", fmt_cmd()))?;

    let utf8_error = |e: FromUtf8Error, kind: &str| {
        anyhow::anyhow!(
            "Command `{}` {} contains invalid UTF-8: {} \n {:?}",
            fmt_cmd(),
            kind,
            e.utf8_error(),
            e.into_bytes()
        )
    };
    let stdout = String::from_utf8(output.stdout).map_err(|e| utf8_error(e, "stdout"))?;
    let stderr = String::from_utf8(output.stderr).map_err(|e| utf8_error(e, "stderr"))?;

    if !output.status.success() {
        anyhow::bail!(
            "Command `{}` failed with {}:\n-- stdout\n{stdout}\n-- stderr\n{stderr}",
            fmt_cmd(),
            output.status,
        );
    }
    Ok((stdout, stderr))
}

/// Joins the items of an [`Iterator`] into a [`String`], with `separator` between each of them.
fn intersperse<S: AsRef<str>>(separator: &str, iter: impl Iterator<Item = S>) -> String {
    let mut out = String::new();
    for (i, item) in iter.enumerate() {
        if i > 0 {
            out.push_str(separator);
        }
        out.push_str(item.as_ref());
    }
    out
}

#[cfg(not(feature = "tty"))]
/// Prompt user if they want to install a new Rust toolchain.
fn get_consent_for_toolchain_install(
    _prompt: &str,
    skip_toolchain_install_consent: bool,
) -> anyhow::Result<()> {
    if skip_toolchain_install_consent {
        Ok(())
    } else {
        no_tty()
    }
}

#[cfg(feature = "tty")]
/// Prompt user if they want to install a new Rust toolchain.
fn get_consent_for_toolchain_install(
    prompt: &str,
    skip_toolchain_install_consent: bool,
) -> anyhow::Result<()> {
    if skip_toolchain_install_consent {
        return Ok(());
    }

    if !std::io::stdout().is_tty() {
        no_tty()
    }

    log::debug!("asking for consent to install the required toolchain");
    crossterm::terminal::enable_raw_mode().context("enabling raw mode")?;
    // Restore the terminal even if reading the input fails below.
    let raw_mode = RawModeGuard;
    user_output!("{prompt} [Y/n]: ");
    let mut input = crossterm::event::read().context("reading crossterm event")?;

    if let crossterm::event::Event::Key(crossterm::event::KeyEvent {
        code: crossterm::event::KeyCode::Enter,
        kind: crossterm::event::KeyEventKind::Release,
        ..
    }) = input
    {
        // In Powershell, programs will potentially observe the Enter key release after they started
        // (see crossterm#124). If that happens, re-read the input.
        input = crossterm::event::read().context("re-reading crossterm event")?;
    }
    drop(raw_mode);

    #[expect(clippy::print_stdout, reason = "need a newline after crossterm input")]
    {
        println!();
    }

    if let crossterm::event::Event::Key(crossterm::event::KeyEvent {
        code: crossterm::event::KeyCode::Char('y') | crossterm::event::KeyCode::Enter,
        ..
    }) = input
    {
        Ok(())
    } else {
        user_output!("Exiting...\n");
        #[expect(clippy::exit, reason = "user requested abort")]
        std::process::exit(0);
    }
}

/// Disables terminal raw mode when dropped.
#[cfg(feature = "tty")]
struct RawModeGuard;

#[cfg(feature = "tty")]
impl Drop for RawModeGuard {
    fn drop(&mut self) {
        if let Err(err) = crossterm::terminal::disable_raw_mode() {
            log::error!("failed to disable terminal raw mode: {err}");
        }
    }
}

fn no_tty() -> ! {
    user_output!("No TTY detected so can't ask for consent to install Rust toolchain.");
    log::error!("Attempted to ask for consent when there's no TTY");
    #[expect(clippy::exit, reason = "can't ask for user consent if there's no TTY")]
    std::process::exit(1);
}

#[cfg(test)]
mod tests {
    use super::*;

    const HOST: &str = "aarch64-unknown-linux-gnu";

    #[test]
    fn toolchain_listed_matches_exact_word_only() {
        let list = "stable-aarch64-unknown-linux-gnu (default)\n\
                    nightly-2024-04-24-aarch64-unknown-linux-gnu (active)\n";
        assert!(is_toolchain_listed(
            list,
            "nightly-2024-04-24-aarch64-unknown-linux-gnu"
        ));
        assert!(!is_toolchain_listed(list, "nightly-2024-04-24"));
        assert!(!is_toolchain_listed("", "stable-aarch64-unknown-linux-gnu"));
    }

    #[test]
    fn all_components_installed() {
        let list = format!(
            "cargo-{HOST}\nclippy-{HOST}\nllvm-tools-{HOST}\nrust-src\n\
             rust-std-{HOST}\nrustc-{HOST}\nrustc-dev-{HOST}\n"
        );
        assert_eq!(missing_components(&list, HOST), HashSet::new());
    }

    #[test]
    fn reports_missing_components() {
        let list = format!("cargo-{HOST}\nrust-src\nrust-std-{HOST}\nrustc-{HOST}\n");
        assert_eq!(
            missing_components(&list, HOST),
            HashSet::from(["clippy", "rustc-dev", "llvm-tools"])
        );
    }

    #[test]
    fn components_of_another_host_dont_count() {
        let list = "rustc-dev-x86_64-pc-windows-msvc\n";
        assert!(missing_components(list, HOST).contains("rustc-dev"));
    }

    #[test]
    fn intersperse_joins() {
        assert_eq!(intersperse(", ", ["a", "b", "c"].into_iter()), "a, b, c");
        assert_eq!(intersperse(", ", ["a"].into_iter()), "a");
        // used to underflow and panic
        assert_eq!(intersperse(", ", std::iter::empty::<&str>()), "");
    }

    /// The command and the stream name used to be swapped in this message.
    #[cfg(unix)]
    #[test]
    fn run_cmd_invalid_utf8_names_command_then_stream() {
        let err = run_cmd(Command::new("printf").arg("\\377")).unwrap_err();
        assert!(
            err.to_string()
                .starts_with(r"Command `printf \377` stdout contains invalid UTF-8"),
            "{err}"
        );
    }

    #[test]
    fn run_cmd_failure_names_the_command() {
        let err = run_cmd(Command::new("cargo").args(["--definitely-not-a-flag"])).unwrap_err();
        assert!(
            err.to_string()
                .starts_with("Command `cargo --definitely-not-a-flag` failed"),
            "{err}"
        );
    }
}
