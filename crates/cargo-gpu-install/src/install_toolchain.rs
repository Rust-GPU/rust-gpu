//! toolchain installation logic

use crate::user_output;
use anyhow::Context as _;
#[cfg(feature = "tty")]
use crossterm::tty::IsTty as _;
use std::collections::HashSet;
use std::process::Command;
use std::string::FromUtf8Error;

/// list of required rustup components
pub const REQUIRED_COMPONENTS: &[&str] =
    ["rust-src", "rustc-dev", "llvm-tools", "clippy"].as_slice();

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
                    "-c",
                    &intersperse(",", REQUIRED_COMPONENTS.iter().copied()),
                ])
                .stdout(std::process::Stdio::inherit())
                .stderr(std::process::Stdio::inherit()),
        )?;
    }

    Ok(())
}

/// Returns true if the toolchain and required components are installed.
fn is_toolchain_installed(toolchain: &str, host_tuple: &str) -> anyhow::Result<bool> {
    // check if toolchain is installed
    // CMD: `rustup toolchain list -q`
    let (list_toolchains, _) = run_cmd(Command::new("rustup").args(["toolchain", "list", "-q"]))?;
    if !list_toolchains
        .split_ascii_whitespace()
        .any(|s| s == toolchain)
    {
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

    // components are listed as:
    // * `llvm-tools-aarch64-unknown-linux-gnu` and we need to snippet off the host tuple from the end
    // * `rust-src` since source code isn't target dependent
    let component_host_suffix = format!("-{host_tuple}");
    let mut required = REQUIRED_COMPONENTS.iter().copied().collect::<HashSet<_>>();
    for component in components.split_ascii_whitespace() {
        required.remove(
            component
                .strip_suffix(&component_host_suffix)
                .unwrap_or(component),
        );
    }
    if !required.is_empty() {
        log::info!("components {required:?} missing for toolchain {toolchain}");
        return Ok(false);
    }

    log::info!("toolchain and required components are already installed");
    Ok(true)
}

pub fn run_cmd(cmd: &mut Command) -> anyhow::Result<(String, String)> {
    let output = cmd.output();
    let fmt_cmd = || {
        intersperse(
            " ",
            std::iter::once(cmd.get_program())
                .chain(cmd.get_args())
                .map(|s| s.to_str().unwrap()),
        )
    };
    let output = output.with_context(|| format!("Failed to launch cmd `{}`", fmt_cmd()))?;

    let utf8_error = |e: FromUtf8Error, kind: &str| {
        anyhow::anyhow!(
            "Command `{}` {} contains invalid UTF-8: {} \n {:?}",
            kind,
            fmt_cmd(),
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
            &output.status,
        );
    }
    Ok((stdout, stderr))
}

/// Folds an [`Iterator`] of `&str` into a [`String`] while interspersing some `&str` between each element
#[expect(clippy::string_add, reason = "Deliberately using String::add")]
fn intersperse<'a>(intersperse: &str, iter: impl Iterator<Item = &'a str>) -> String {
    let mut s = iter.fold(String::new(), |a, b| a + b + intersperse);
    s.truncate(s.len() - intersperse.len());
    s
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
    crate::user_output!("{prompt} [y/n]: ");
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
    crossterm::terminal::disable_raw_mode().context("disabling raw mode")?;

    if let crossterm::event::Event::Key(crossterm::event::KeyEvent {
        code: crossterm::event::KeyCode::Char('y'),
        ..
    }) = input
    {
        Ok(())
    } else {
        crate::user_output!("Exiting...\n");
        #[expect(clippy::exit, reason = "user requested abort")]
        std::process::exit(0);
    }
}

fn no_tty() -> ! {
    user_output!("No TTY detected so can't ask for consent to install Rust toolchain.");
    log::error!("Attempted to ask for consent when there's no TTY");
    #[expect(clippy::exit, reason = "can't ask for user consent if there's no TTY")]
    std::process::exit(1);
}
