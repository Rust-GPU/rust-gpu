use clap::Parser;
use compiletest::common::Mode;
use itertools::Itertools as _;
use rustc_codegen_spirv_types::{SpirvTarget, TargetSpec, TargetSpecVersion, query_rustc_version};
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
            Self::SpirvLib => format!("{}/debug/build", target.target()),
            Self::ProcMacro => "debug/build".into(),
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
    let target_spec_dir = original_target_dir.join("compiletest-target-spec");
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
        target_spec_dir,
        codegen_backend_path,
    };

    runner.run_mode(Mode::Ui);
}

struct Runner {
    opt: Opt,
    tests_dir: PathBuf,
    compiletest_build_dir: PathBuf,
    deps_target_dir: PathBuf,
    target_spec_dir: PathBuf,
    codegen_backend_path: PathBuf,
}

impl Runner {
    /// Runs the given `mode` on the directory that matches that name, using the
    /// backend provided by `codegen_backend_path`.
    #[allow(clippy::string_add)]
    fn run_mode(&self, mode: Mode) {
        /// RUSTFLAGS passed to all test files.
        fn test_rustc_flags(
            codegen_backend_path: &Path,
            deps: &[TestDep],
            search_dirs: &[PathBuf],
        ) -> String {
            [
                &*rust_flags(codegen_backend_path),
                &*search_dirs
                    .iter()
                    .map(|dir| format!("-L dependency={}", dir.display()))
                    .join(" "),
                "--edition 2021",
                &*deps.iter().map(TestDep::to_rustc_extern).join(" "),
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

            let target = SpirvTarget::parse(env).unwrap();
            let rustc_version = query_rustc_version(None).unwrap();
            let target_spec =
                TargetSpecVersion::target_arg(rustc_version, &target, &self.target_spec_dir)
                    .unwrap();

            let libs = self.build_deps(&target, &target_spec);
            let search_dirs = self.dep_search_dirs(&target);
            let mut flags = test_rustc_flags(&self.codegen_backend_path, &libs, &search_dirs);
            flags += variation.extra_flags;

            let config = compiletest::Config {
                stage_id,
                target_rustcflags: Some(flags),
                mode,
                target: target_spec.target.into_string().unwrap(),
                src_base: self.tests_dir.join(mode.to_string()),
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
    fn build_deps(&self, target: &SpirvTarget, target_spec: &TargetSpec) -> Vec<TestDep> {
        // Build compiletests-deps-helper
        let mut cmd = std::process::Command::new("cargo");
        cmd.args([
            "build",
            "-p",
            "compiletests-deps-helper",
            "-Zbuild-std=core",
            "-Zbuild-std-features=compiler-builtins-mem",
            "-Zbuild-dir-new-layout",
        ]);
        target_spec.append_to_cmd(&mut cmd);
        cmd.arg("--target-dir")
            .arg(&self.deps_target_dir)
            .env("RUSTFLAGS", rust_flags(&self.codegen_backend_path))
            .stderr(std::process::Stdio::inherit())
            .stdout(std::process::Stdio::inherit())
            .status()
            .and_then(map_status_to_result)
            .unwrap();

        let all_deps: Result<_, ()> = (|| {
            Ok([
                self.find_lib("compiler_builtins", DepKind::SpirvLib, target)?
                    .no_prelude(),
                self.find_lib("core", DepKind::SpirvLib, target)?
                    .no_prelude(),
                self.find_lib("spirv-std", DepKind::SpirvLib, target)?,
                self.find_lib("glam", DepKind::SpirvLib, target)?,
                self.find_lib("spirv-std-macros", DepKind::ProcMacro, target)?,
            ])
        })();
        if let Ok(all_deps) = all_deps {
            Vec::from(all_deps)
        } else {
            eprintln!("warning: cleaning and rebuilding deps");
            self.clean_deps();
            self.build_deps(target, target_spec)
        }
    }

    fn clean_deps(&self) {
        std::process::Command::new("cargo")
            .arg("clean")
            .arg("--target-dir")
            .arg(&self.deps_target_dir)
            .status()
            .and_then(map_status_to_result)
            .unwrap();
    }
}

impl Runner {
    /// search for `out` dirs for all compiled libraries
    fn dep_search_dirs(&self, target: &SpirvTarget) -> Vec<PathBuf> {
        [
            self.deps_target_dir
                .join(DepKind::SpirvLib.target_dir_suffix(target)),
            self.deps_target_dir
                .join(DepKind::ProcMacro.target_dir_suffix(target)),
        ]
        .iter()
        .filter_map(|build_dir| std::fs::read_dir(build_dir).ok())
        .flatten()
        .filter_map(|crate_dir| std::fs::read_dir(crate_dir.ok()?.path()).ok())
        .flatten()
        .filter_map(|hash_dir| {
            let out_dir = hash_dir.ok()?.path().join("out");
            out_dir.is_dir().then_some(out_dir)
        })
        .collect()
    }

    /// Attempt find the rlib that matches `base`, if multiple rlibs are found then
    /// a clean build is required and `Err(FindLibError::Duplicate)` is returned.
    fn find_lib(&self, name: &str, dep_kind: DepKind, target: &SpirvTarget) -> Result<TestDep, ()> {
        let ident_name = name.replace("-", "_");
        let (expected_prefix, expected_suffix) = dep_kind.prefix_and_extension();
        let expected_prefix = format!("{expected_prefix}{}", ident_name);
        let build_dir = self
            .deps_target_dir
            .join(dep_kind.target_dir_suffix(target))
            .join(name);

        let rlib = std::fs::read_dir(&build_dir)
            .unwrap_or_else(|_| panic!("Couldn't read dir {}", build_dir.display()))
            .filter_map(|entry| {
                let out_dir = entry.ok()?.path().join("out");
                std::fs::read_dir(out_dir).ok()
            })
            .flatten()
            .filter_map(|entry| {
                let path = entry.ok()?.path();
                let file_name = path.file_name()?.to_str()?;
                (file_name.starts_with(&expected_prefix) && file_name.ends_with(expected_suffix))
                    .then_some(path)
            })
            .exactly_one()
            .map_err(|_e| ())?;
        Ok(TestDep::new(ident_name, rlib))
    }
}

struct TestDep {
    name: String,
    rlib: PathBuf,
    no_prelude: bool,
}

impl TestDep {
    pub fn new(name: String, rlib: PathBuf) -> Self {
        Self {
            name,
            rlib,
            no_prelude: false,
        }
    }

    pub fn no_prelude(self) -> Self {
        Self {
            no_prelude: true,
            ..self
        }
    }

    pub fn to_rustc_extern(&self) -> String {
        let noprelude = if self.no_prelude { "noprelude:" } else { "" };
        format!("--extern {noprelude}{}={}", self.name, self.rlib.display())
    }
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
