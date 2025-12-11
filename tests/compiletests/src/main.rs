use clap::Parser;
use itertools::Itertools as _;
use rustc_codegen_spirv_types::{SpirvTarget, TargetSpecVersion, query_rustc_version};
use std::ffi::OsString;
use std::{
    env, io,
    path::{Path, PathBuf},
};

#[derive(Parser)]
#[command(bin_name = "cargo compiletest")]
struct Opt {
    /// Automatically update stderr/stdout files.
    #[arg(long)]
    bless: bool,

    /// The environment to compile to the SPIR-V tests.
    #[arg(long, default_value = "vulkan1.2")]
    target_env: String,

    /// Only run tests that match these filters.
    #[arg(name = "FILTER")]
    filters: Vec<String>,
}

impl Opt {
    pub fn environments(&self) -> impl Iterator<Item = &str> {
        self.target_env.split(',')
    }
}

#[derive(Copy, Clone)]
enum DepKind {
    SpirvLib,
    ProcMacro,
}

impl DepKind {
    fn prefix_and_extension(self) -> (&'static str, &'static str) {
        match self {
            Self::SpirvLib => ("lib", "rlib"),
            Self::ProcMacro => (env::consts::DLL_PREFIX, env::consts::DLL_EXTENSION),
        }
    }

    fn target_dir_suffix(self, target: &SpirvTarget) -> String {
        match self {
            Self::SpirvLib => format!("{}/debug/deps", target.target()),
            Self::ProcMacro => "debug/deps".into(),
        }
    }
}

fn main() {
    let opt = Opt::parse();

    // Pull in rustc_codegen_spirv as a dynamic library in the same way
    // spirv-builder does.
    let codegen_backend_path = find_rustc_codegen_spirv();

    let tests_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = tests_dir.parent().unwrap().parent().unwrap();
    let original_target_dir = workspace_root.join("target");
    let deps_target_dir = original_target_dir.join("compiletest-deps");
    let compiletest_build_dir = original_target_dir.join("compiletest-results");

    // HACK(eddyb) force `compiletest` to pass `ui/...` relative paths to `rustc`,
    // which should always end up being the same regardless of the path that the
    // Rust-GPU repo is checked out at (among other things, this avoids harcoded
    // `compiletest` limits being hit by e.g. users with slightly longer paths).
    std::env::set_current_dir(tests_dir).unwrap();
    let tests_dir = PathBuf::from("");

    let runner = Runner {
        opt,
        tests_dir,
        compiletest_build_dir,
        deps_target_dir,
        codegen_backend_path,
    };

    runner.run_mode("ui");
}

struct Runner {
    opt: Opt,
    tests_dir: PathBuf,
    compiletest_build_dir: PathBuf,
    deps_target_dir: PathBuf,
    codegen_backend_path: PathBuf,
}

impl Runner {
    /// Runs the given `mode` on the directory that matches that name, using the
    /// backend provided by `codegen_backend_path`.
    #[allow(clippy::string_add)]
    fn run_mode(&self, mode: &'static str) {
        /// RUSTFLAGS passed to all test files.
        fn test_rustc_flags(
            codegen_backend_path: &Path,
            deps: &TestDeps,
            indirect_deps_dirs: &[&Path],
        ) -> String {
            [
                &*rust_flags(codegen_backend_path),
                &*indirect_deps_dirs
                    .iter()
                    .map(|dir| format!("-L dependency={}", dir.display()))
                    .fold(String::new(), |a, b| b + " " + &a),
                "--edition 2021",
                &*format!("--extern noprelude:core={}", deps.core.display()),
                &*format!(
                    "--extern noprelude:compiler_builtins={}",
                    deps.compiler_builtins.display()
                ),
                &*format!(
                    "--extern spirv_std_macros={}",
                    deps.spirv_std_macros.display()
                ),
                &*format!("--extern spirv_std={}", deps.spirv_std.display()),
                &*format!("--extern glam={}", deps.glam.display()),
                "--crate-type dylib",
                "-Zunstable-options",
                "-Zcrate-attr=no_std",
                "-Zcrate-attr=feature(asm_experimental_arch)",
                "-Zui-testing",
            ]
            .join(" ")
        }

        struct Variation {
            name: &'static str,
            extra_flags: &'static str,
        }
        const VARIATIONS: &[Variation] = &[Variation {
            name: "default",
            extra_flags: "",
        }];

        for (env, variation) in self
            .opt
            .environments()
            .flat_map(|env| VARIATIONS.iter().map(move |variation| (env, variation)))
        {
            // HACK(eddyb) in order to allow *some* tests to have separate output
            // in different testing variations (i.e. experimental features), while
            // keeping *most* of the tests unchanged, we make use of "stage IDs",
            // which offer `// only-S` and `// ignore-S` for any stage ID `S`.
            let stage_id = if variation.name == "default" {
                // Use the environment name as the stage ID.
                env.to_string()
            } else {
                // Include the variation name in the stage ID.
                format!("{}-{}", env, variation.name)
            };

            println!("Testing env: {stage_id}\n");

            let target = SpirvTarget::from_env(env).unwrap();
            let libs = self.build_deps(&target);
            let mut flags = test_rustc_flags(
                &self.codegen_backend_path,
                &libs,
                &[
                    &self
                        .deps_target_dir
                        .join(DepKind::SpirvLib.target_dir_suffix(&target)),
                    &self
                        .deps_target_dir
                        .join(DepKind::ProcMacro.target_dir_suffix(&target)),
                ],
            );
            flags += variation.extra_flags;

            let config = compiletest::Config {
                stage_id,
                target_rustcflags: Some(flags),
                mode: mode.parse().expect("Invalid mode"),
                target: self.target_spec_json(&target).into_string().unwrap(),
                src_base: self.tests_dir.join(mode),
                build_base: self.compiletest_build_dir.clone(),
                bless: self.opt.bless,
                filters: self.opt.filters.clone(),
                ..compiletest::Config::default()
            };
            // FIXME(eddyb) do we need this? shouldn't `compiletest` be independent?
            config.clean_rmeta();

            compiletest::run_tests(&config);
        }
    }

    /// Runs the processes needed to build `spirv-std` & other deps.
    fn build_deps(&self, target: &SpirvTarget) -> TestDeps {
        // Build compiletests-deps-helper
        std::process::Command::new("cargo")
            .args([
                "build",
                "-p",
                "compiletests-deps-helper",
                "-Zbuild-std=core",
                "-Zbuild-std-features=compiler-builtins-mem",
                "--target",
            ])
            .arg(self.target_spec_json(target))
            .arg("--target-dir")
            .arg(&self.deps_target_dir)
            .env("RUSTFLAGS", rust_flags(&self.codegen_backend_path))
            .stderr(std::process::Stdio::inherit())
            .stdout(std::process::Stdio::inherit())
            .status()
            .and_then(map_status_to_result)
            .unwrap();

        let compiler_builtins = self.find_lib("compiler_builtins", DepKind::SpirvLib, target);
        let core = self.find_lib("core", DepKind::SpirvLib, target);
        let spirv_std = self.find_lib("spirv_std", DepKind::SpirvLib, target);
        let glam = self.find_lib("glam", DepKind::SpirvLib, target);
        let spirv_std_macros = self.find_lib("spirv_std_macros", DepKind::ProcMacro, target);

        let all_libs = [
            &compiler_builtins,
            &core,
            &spirv_std,
            &glam,
            &spirv_std_macros,
        ];
        if all_libs.iter().any(|r| r.is_err()) {
            // FIXME(eddyb) `missing_count` should always be `0` anyway.
            // FIXME(eddyb) use `--message-format=json-render-diagnostics` to
            // avoid caring about duplicates (or search within files at all).
            let missing_count = all_libs
                .iter()
                .filter(|r| matches!(r, Err(FindLibError::Missing)))
                .count();
            let duplicate_count = all_libs
                .iter()
                .filter(|r| matches!(r, Err(FindLibError::Duplicate)))
                .count();
            eprintln!(
                "warning: cleaning deps ({missing_count} missing libs, {duplicate_count} duplicated libs)"
            );
            self.clean_deps();
            self.build_deps(target)
        } else {
            TestDeps {
                core: core.ok().unwrap(),
                glam: glam.ok().unwrap(),
                compiler_builtins: compiler_builtins.ok().unwrap(),
                spirv_std: spirv_std.ok().unwrap(),
                spirv_std_macros: spirv_std_macros.ok().unwrap(),
            }
        }
    }

    fn clean_deps(&self) {
        std::process::Command::new("cargo")
            .arg("clean")
            .arg("--target-dir")
            .arg(&self.deps_target_dir)
            .stderr(std::process::Stdio::inherit())
            .stdout(std::process::Stdio::inherit())
            .status()
            .and_then(map_status_to_result)
            .unwrap();
    }

    fn target_spec_json(&self, target: &SpirvTarget) -> OsString {
        let rustc_version = query_rustc_version(None).unwrap();
        TargetSpecVersion::target_arg(rustc_version, target, &self.deps_target_dir).unwrap()
    }
}

enum FindLibError {
    Missing,
    Duplicate,
}

impl Runner {
    /// Attempt find the rlib that matches `base`, if multiple rlibs are found then
    /// a clean build is required and `Err(FindLibError::Duplicate)` is returned.
    fn find_lib(
        &self,
        base: impl AsRef<Path>,
        dep_kind: DepKind,
        target: &SpirvTarget,
    ) -> Result<PathBuf, FindLibError> {
        let base = base.as_ref();
        let (expected_prefix, expected_extension) = dep_kind.prefix_and_extension();
        let expected_name = format!("{}{}", expected_prefix, base.display());

        let dir = self
            .deps_target_dir
            .join(dep_kind.target_dir_suffix(target));

        std::fs::read_dir(dir)
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(move |path| {
                let name = {
                    let name = path.file_stem();
                    if name.is_none() {
                        return false;
                    }
                    name.unwrap()
                };

                let name_matches = name.to_str().unwrap().starts_with(&expected_name)
                && name.len() == expected_name.len() + 17   // we expect our name, '-', and then 16 hexadecimal digits
                && ends_with_dash_hash(name.to_str().unwrap());
                let extension_matches = path
                    .extension()
                    .is_some_and(|ext| ext == expected_extension);

                name_matches && extension_matches
            })
            .exactly_one()
            .map_err(|mut iter| {
                if iter.next().is_none() {
                    FindLibError::Missing
                } else {
                    FindLibError::Duplicate
                }
            })
    }
}

/// Returns whether this string ends with a dash ('-'), followed by 16 lowercase hexadecimal characters
fn ends_with_dash_hash(s: &str) -> bool {
    let n = s.len();
    if n < 17 {
        return false;
    }
    let mut bytes = s.bytes().skip(n - 17);
    if bytes.next() != Some(b'-') {
        return false;
    }

    bytes.all(|b| b.is_ascii_hexdigit())
}

/// Paths to all of the library artifacts of dependencies needed to compile tests.
struct TestDeps {
    core: PathBuf,
    compiler_builtins: PathBuf,
    spirv_std: PathBuf,
    spirv_std_macros: PathBuf,
    glam: PathBuf,
}

/// The RUSTFLAGS passed to all SPIR-V builds.
// FIXME(eddyb) expose most of these from `spirv-builder`.
fn rust_flags(codegen_backend_path: &Path) -> String {
    [
        &*format!("-Zcodegen-backend={}", codegen_backend_path.display()),
        // Ensure the codegen backend is emitted in `.d` files to force Cargo
        // to rebuild crates compiled with it when it changes (this used to be
        // the default until https://github.com/rust-lang/rust/pull/93969).
        "-Zbinary-dep-depinfo",
        "-Csymbol-mangling-version=v0",
        "-Zcrate-attr=feature(register_tool)",
        "-Zcrate-attr=register_tool(rust_gpu)",
        // HACK(eddyb) this is the same configuration that we test with, and
        // ensures no unwanted surprises from e.g. `core` debug assertions.
        "-Coverflow-checks=off",
        "-Cdebug-assertions=off",
        // HACK(eddyb) we need this for `core::fmt::rt::Argument::new_*` calls
        // to *never* be inlined, so we can pattern-match the calls themselves.
        "-Zinline-mir=off",
        // HACK(eddyb) similar to turning MIR inlining off, we also can't allow
        // optimizations that drastically impact (the quality of) codegen, and
        // GVN currently can lead to the memcpy-out-of-const-alloc-global-var
        // pattern, even for `ScalarPair` (e.g. `return None::<u32>;`).
        "-Zmir-enable-passes=-GVN",
        // HACK(eddyb) avoid ever reusing instantiations from `compiler_builtins`
        // which is special-cased to turn calls to functions that never return,
        // into aborts, and this applies to the panics of UB-checking helpers
        // (https://github.com/rust-lang/rust/pull/122580#issuecomment-3033026194)
        // but while upstream that only loses the panic message, for us it's even
        // worse, as we lose the chance to remove otherwise-dead `fmt::Arguments`.
        "-Zshare-generics=off",
        // NOTE(eddyb) flags copied from `spirv-builder` are all above this line.
        "-Cdebuginfo=2",
        "-Cembed-bitcode=no",
    ]
    .join(" ")
}

/// Convenience function to map process failure to results in Rust.
fn map_status_to_result(status: std::process::ExitStatus) -> io::Result<()> {
    match status.success() {
        true => Ok(()),
        false => Err(io::Error::other(format!(
            "process terminated with non-zero code: {}",
            status.code().unwrap_or(0)
        ))),
    }
}

// https://github.com/rust-lang/cargo/blob/1857880b5124580c4aeb4e8bc5f1198f491d61b1/src/cargo/util/paths.rs#L29-L52
fn dylib_path_envvar() -> &'static str {
    if cfg!(windows) {
        "PATH"
    } else if cfg!(target_os = "macos") {
        "DYLD_FALLBACK_LIBRARY_PATH"
    } else {
        "LD_LIBRARY_PATH"
    }
}

fn dylib_path() -> Vec<PathBuf> {
    let mut dylibs = match env::var_os(dylib_path_envvar()) {
        Some(var) => env::split_paths(&var).collect(),
        None => Vec::new(),
    };
    if let Ok(dir) = env::current_dir() {
        dylibs.push(dir);
    }
    dylibs
}

fn find_rustc_codegen_spirv() -> PathBuf {
    let filename = format!(
        "{}rustc_codegen_spirv{}",
        env::consts::DLL_PREFIX,
        env::consts::DLL_SUFFIX
    );
    let dylib_paths = dylib_path();
    for mut path in dylib_paths {
        path.push(&filename);
        if path.is_file() {
            return path;
        }
    }
    panic!("Could not find {filename} in library path");
}
