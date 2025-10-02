//! This custom build script checks whether we're compiling with the appropriate
//! Rust toolchain, and also handles patching `rustc_codegen_ssa` to work around
//! pre-`qptr`-transition limitations (search `pqp_cg_ssa` for affected places).

#![allow(clippy::string_add)]

use std::collections::VecDeque;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::string::ToString;
use std::{env, fs, mem};

/// Current `rust-toolchain.toml` file
/// WARNING!!! cargo-gpu is now relying on this being a string literal! It will
/// scan `build.rs` for any line starting with `channel = "..."` to figure out
/// which toolchain version to use! This also allows backwards compat.
/// Unfortunately, directly including the actual workspace `rust-toolchain.toml` doesn't work together with
/// `cargo publish`. We need to figure out a way to do this properly, but let's hardcode it for now :/
//const REQUIRED_RUST_TOOLCHAIN: &str = include_str!("../../rust-toolchain.toml");
const REQUIRED_RUST_TOOLCHAIN: &str = r#"[toolchain]
channel = "nightly-2025-08-04"
components = ["rust-src", "rustc-dev", "llvm-tools"]
# commit_hash = f34ba774c78ea32b7c40598b8ad23e75cdac42a6"#;

fn rustc_output(arg: &str) -> Result<String, Box<dyn Error>> {
    let rustc = env::var("RUSTC").unwrap_or_else(|_| "rustc".into());
    Ok(String::from_utf8(
        Command::new(rustc).arg(arg).output()?.stdout,
    )?)
}

fn get_rustc_commit_hash() -> Result<String, Box<dyn Error>> {
    rustc_output("-vV")?
        .lines()
        .find_map(|l| l.strip_prefix("commit-hash: "))
        .map(ToString::to_string)
        .ok_or_else(|| "`commit-hash` not found in `rustc -vV` output".into())
}

fn get_required_commit_hash() -> Result<String, Box<dyn Error>> {
    REQUIRED_RUST_TOOLCHAIN
        .lines()
        .find_map(|l| l.strip_prefix("# commit_hash = "))
        .map(ToString::to_string)
        .ok_or_else(|| "`commit_hash` not found in `rust-toolchain.toml`".into())
}

fn check_toolchain_version() -> Result<(), Box<dyn Error>> {
    // make sure we rebuild if RUSTGPU_SKIP_TOOLCHAIN_CHECK env var changes
    println!("cargo:rerun-if-env-changed=RUSTGPU_SKIP_TOOLCHAIN_CHECK");

    // if we're building from local source, check if REQUIRED_RUST_TOOLCHAIN matches ../../rust-toolchain.toml
    if env::current_dir()?.ends_with("crates/rustc_codegen_spirv") {
        let current_toolchain = std::fs::read_to_string("../../rust-toolchain.toml")?;
        if !current_toolchain.contains(REQUIRED_RUST_TOOLCHAIN) {
            return Err(format!(
                "error: building from local source while `REQUIRED_RUST_TOOLCHAIN` (defined in `{}`) doesn't match `{}`",
                file!(),
                Path::new("../../rust-toolchain.toml")
                    .canonicalize()?
                    .display()
            ).into());
        }
    }

    if !cfg!(feature = "skip-toolchain-check") && env::var("RUSTGPU_SKIP_TOOLCHAIN_CHECK").is_err()
    {
        // check if our current rustc's commit hash matches with what we expect it to be
        let current_hash = get_rustc_commit_hash()?;
        let required_hash = get_required_commit_hash()?;
        if current_hash != required_hash {
            let stripped_toolchain = REQUIRED_RUST_TOOLCHAIN
                .lines()
                .filter(|l| !l.trim().is_empty() && !l.starts_with("# "))
                .map(ToString::to_string)
                .reduce(|a, b| a + "\n" + &b)
                .unwrap_or_default();

            return Err(format!(
                "error: wrong toolchain detected (found commit hash `{current_hash}`, expected `{required_hash}`).
Make sure your `rust-toolchain.toml` file contains the following:
-------------
{stripped_toolchain}
-------------"
            ).into());
        }
    }

    Ok(())
}

/// Copy `rustc_codegen_ssa` (from the `rustc-dev` component) and patch it to
/// produce a "pqp" ("pre-`qptr`-patched") version that maintains compatibility
/// with "legacy" Rust-GPU pointer handling (mainly typed `alloca`s).
//
// FIXME(eddyb) get rid of this as soon as it's not needed anymore.
fn generate_pqp_cg_ssa() -> Result<(), Box<dyn Error>> {
    let sysroot = rustc_output("--print=sysroot")?;
    let sysroot = Path::new(sysroot.trim());
    let cg_ssa_dir = sysroot.join("lib/rustlib/rustc-src/rust/compiler/rustc_codegen_ssa");
    if !cg_ssa_dir.is_dir() {
        return Err(format!(
            "missing `rustc-dev` component from toolchain `{}` (at {})",
            env::var("RUSTUP_TOOLCHAIN").unwrap_or_else(|_| "<unknown>".into()),
            sysroot.display(),
        )
        .into());
    }

    let mut cg_ssa_lib_rc_attrs = String::new();
    let mut cg_ssa_lib_rs_extern_crates = String::new();
    let writeln = |s: &mut String, line: &str| {
        *s += line;
        s.push('\n');
    };

    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let out_pqp_cg_ssa_dir = out_dir.join("pqp_cg_ssa");
    let _ = fs::remove_dir_all(&out_pqp_cg_ssa_dir);

    let mut queue = VecDeque::new();
    queue.push_back(PathBuf::new());
    while let Some(suffix) = queue.pop_front() {
        let in_dir = cg_ssa_dir.join(&suffix);
        let out_dir = out_pqp_cg_ssa_dir.join(&suffix);
        fs::create_dir_all(&out_dir)?;

        for entry in in_dir.read_dir()? {
            let entry = entry?;
            let relative_path = suffix.join(entry.file_name());
            if entry.file_type()?.is_dir() {
                queue.push_back(relative_path);
                continue;
            }

            let in_path = entry.path();
            let out_path = out_dir.join(entry.file_name());

            let mut src = fs::read_to_string(in_path)?;

            // FIXME(eddyb) `regex` crate might be more efficient here.
            src = src.replace("crate::", "crate::maybe_pqp_cg_ssa::");
            // HACK(eddyb) needed for `fluent` diagnostic identifiers.
            src = src.replace("codegen_ssa_", "codegen_spirv_");

            if relative_path == Path::new("src/lib.rs") {
                // HACK(eddyb) rewrite line-by-line to comment (and copy) some lines.
                for line in mem::take(&mut src).lines() {
                    if line.starts_with("#!") {
                        src += "// ";
                        if !line.starts_with("#![doc(") && line != "#![warn(unreachable_pub)]" {
                            writeln(&mut cg_ssa_lib_rc_attrs, line);
                        }
                    } else if line == "#[macro_use]" || line.starts_with("extern crate ") {
                        src += "// ";
                        writeln(&mut cg_ssa_lib_rs_extern_crates, line);
                    }
                    writeln(&mut src, line);
                }
            }

            if relative_path == Path::new("src/back/link.rs") {
                // HACK(eddyb) remove `windows` dependency (from MSVC linker output
                // parsing, which `rustc_codegen_spirv` will never invoke anyway).
                src = src.replace(
                    "#[cfg(not(windows))]
fn escape_linker_output(",
                    "fn escape_linker_output(",
                );
                src = src.replace(
                    "#[cfg(windows)]
fn escape_linker_output(",
                    "#[cfg(any())]
fn escape_linker_output(",
                );
                src = src.replace(
                    "#[cfg(windows)]
mod win {",
                    "#[cfg(any())]
mod win {",
                );
                // HACK(eddyb) remove `object` dependency (for Windows `raw_dylib`
                // handling, which `rustc_codegen_spirv` will never invoke anyway).
                src = src.replace("mod raw_dylib;", "// mod raw_dylib;");
                src = src.replace(
                    "
        for output_path in raw_dylib::",
                    "
        #[cfg(any())]
        for output_path in raw_dylib::",
                );
                src = src.replace(
                    "
        for link_path in raw_dylib::",
                    "
        #[cfg(any())]
        for link_path in raw_dylib::",
                );
            }
            if relative_path == Path::new("src/back/metadata.rs") {
                // HACK(eddyb) remove `object` dependency.
                src = src.replace(
                    "
pub(crate) fn create_object_file(sess: &Session) -> Option<write::Object<'static>> {",
                    "
pub(crate) fn create_object_file(_: &Session) -> Option<write::Object<'static>> {
    None
}
#[cfg(any())]
pub(crate) fn create_object_file(sess: &Session) -> Option<write::Object<'static>> {",
                );
                src = src.replace(
                    "
pub(super) fn elf_e_flags(architecture: Architecture, sess: &Session) -> u32 {",
                    "
#[cfg(any())]
pub(super) fn elf_e_flags(architecture: Architecture, sess: &Session) -> u32 {",
                );
            }

            // HACK(eddyb) "typed alloca" patches.
            if relative_path == Path::new("src/traits/builder.rs") {
                src = src.replace(
                    "
    fn alloca(",
                    "
    fn typed_alloca(&mut self, ty: Self::Type, align: Align) -> Self::Value;
    fn alloca(",
                );
            } else if relative_path == Path::new("src/mir/place.rs") {
                src = src.replace(
                    "Self::alloca_size(bx, layout.size, layout)",
                    "PlaceValue::new_sized(bx.typed_alloca(bx.cx().backend_type(layout), layout.align.abi), layout.align.abi).with_type(layout)",
                );
            } else if relative_path == Path::new("src/mir/operand.rs") {
                src = src.replace("alloca(field.size,", "typed_alloca(llfield_ty,");
            }

            fs::write(out_path, src)?;
        }
    }

    // HACK(eddyb) very basic extraction of deps from original `Cargo.toml`.
    let mut all_extern_crates = cg_ssa_lib_rs_extern_crates;
    let cg_ssa_cargo_toml = fs::read_to_string(out_pqp_cg_ssa_dir.join("Cargo.toml"))?;
    let mut toml_directive = None;
    for line in cg_ssa_cargo_toml.lines() {
        let line = line.trim();
        if line.starts_with('#') || line.is_empty() {
            continue;
        }
        if line.starts_with('[') {
            toml_directive = Some(line);
        } else if toml_directive == Some("[dependencies]")
            && let Some((name, _)) = line.split_once(" = ")
        {
            // HACK(eddyb) ignore a weird edge case.
            if name == "thorin-dwp" {
                continue;
            }
            let extern_crate = format!("extern crate {};", name.replace('-', "_"));
            if !all_extern_crates.contains(&extern_crate) {
                writeln(&mut all_extern_crates, "#[allow(unused_extern_crates)]");
                writeln(&mut all_extern_crates, &extern_crate);
            }
        }
    }

    // HACK(eddyb) warn if `rustc_codegen_spirv`'s `lib.rs` lacks crate attrs.
    let expected_lib_rs_header = format!(
        "\
// HACK(eddyb) start of `rustc_codegen_ssa` crate-level attributes (see `build.rs`).
{cg_ssa_lib_rc_attrs}\
// HACK(eddyb) end of `rustc_codegen_ssa` crate-level attributes (see `build.rs`).
"
    );
    let lib_rs_path = Path::canonicalize(Path::new("src/lib.rs"))?;
    let lib_rs_src = fs::read_to_string(&lib_rs_path)?;
    let lib_rs_has_header = lib_rs_src.starts_with(&expected_lib_rs_header);
    if !lib_rs_has_header {
        println!(
            "cargo::warning={} pqp_cg_ssa header in {}",
            if lib_rs_src.starts_with(expected_lib_rs_header.lines().next().unwrap()) {
                "outdated"
            } else {
                "missing"
            },
            lib_rs_path.display(),
        );
        println!("cargo::warning=(compilation may fail if these attributes don't match)");
        println!("cargo::warning=");
        for line in expected_lib_rs_header.lines() {
            println!("cargo::warning={line}");
        }
        println!("cargo::warning=");

        // HACK(eddyb) allow the warning to be cleared after `lib.rs` is fixed.
        println!("cargo:rerun-if-changed=src/lib.rs");
    }

    // HACK(eddyb) write a file that can be `include!`d from `lib.rs`.
    let pqp_cg_ssa_top_level = all_extern_crates
        + r#"

// HACK(eddyb) reexporting macro output for further macro use (can't patch macro).
use maybe_pqp_cg_ssa::fluent_generated;

#[allow(unused, clippy::all, clippy::pedantic, clippy::restriction)]
#[path = "pqp_cg_ssa/src/lib.rs"]
mod maybe_pqp_cg_ssa;
"#;
    fs::write(out_dir.join("pqp_cg_ssa.rs"), pqp_cg_ssa_top_level)?;

    println!("cargo::rustc-check-cfg=cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)");

    // HACK(eddyb) `if cfg!(llvm_enzyme)` added upstream for autodiff support.
    println!("cargo::rustc-check-cfg=cfg(llvm_enzyme)");

    Ok(())
}

fn main() -> ExitCode {
    match check_toolchain_version().and_then(|()| generate_pqp_cg_ssa()) {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprint!("{e}");
            ExitCode::FAILURE
        }
    }
}
