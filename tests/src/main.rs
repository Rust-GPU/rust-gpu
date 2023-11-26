use clap::Parser;
use itertools::Itertools as _;
use std::{
    borrow::Cow,
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
    #[arg(long, default_value = "spv1.3")]
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

const SPIRV_TARGET_PREFIX: &str = "spirv-unknown-";

fn target_spec_json(target: &str) -> String {
    format!(
        "{}/../crates/spirv-builder/target-specs/{target}.json",
        env!("CARGO_MANIFEST_DIR")
    )
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

    fn target_dir_suffix(self, target: &str) -> String {
        match self {
            Self::SpirvLib => format!("{target}/debug/deps"),
            Self::ProcMacro => "debug/deps".into(),
        }
    }
}

fn main() {
    let opt = Opt::parse();

    let tests_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = tests_dir.parent().unwrap();
    let original_target_dir = workspace_root.join("target");
    let deps_target_dir = original_target_dir.join("compiletest-deps");
    let compiletest_build_dir = original_target_dir.join("compiletest-results");

    let runner = Runner {
        opt,
        tests_dir,
        compiletest_build_dir,
        deps_target_dir,
    };

    runner.run_mode("ui");
}

struct Runner {
    opt: Opt,
    tests_dir: PathBuf,
    compiletest_build_dir: PathBuf,
    deps_target_dir: PathBuf,
}

impl Runner {
    /// Runs the given `mode` on the directory that matches that name.
    #[allow(clippy::string_add)]
    fn run_mode(&self, mode: &'static str) {
        /// RUSTFLAGS passed to all test files.
        fn test_rustc_flags(deps: &TestDeps, indirect_deps_dirs: &[&Path]) -> String {
            SPIRV_BUILD_RUSTFLAGS
                .iter()
                .copied()
                .chain([
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
                    "-Zcrate-attr=feature(asm_const,asm_experimental_arch)",
                ])
                .collect::<Vec<_>>()
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

            let target = format!("{SPIRV_TARGET_PREFIX}{env}");
            let libs = build_deps(&self.deps_target_dir, &target);
            let mut flags = test_rustc_flags(&libs, &[
                &self
                    .deps_target_dir
                    .join(DepKind::SpirvLib.target_dir_suffix(&target)),
                &self
                    .deps_target_dir
                    .join(DepKind::ProcMacro.target_dir_suffix(&target)),
            ]);
            flags += variation.extra_flags;

            let config = compiletest::Config {
                stage_id,
                target_rustcflags: Some(flags),
                mode: mode.parse().expect("Invalid mode"),
                target: target_spec_json(&target),
                src_base: self.tests_dir.join(mode),
                build_base: self.compiletest_build_dir.clone(),
                bless: self.opt.bless,
                filters: self.opt.filters.clone(),
                ..compiletest::Config::default()
            };
            // FIXME(eddyb) do we need this? shouldn't `compiletest` be independent?
            config.clean_rmeta();

            // HACK(eddyb) there isn't a nicer way to force the correct `rustc`.
            if let Some(toolchain) = spirv_builder::codegen_backend::rustup_toolchain_override() {
                env::set_var("RUSTUP_TOOLCHAIN", toolchain);
            }

            compiletest::run_tests(&config);
        }
    }
}

/// Runs the processes needed to build `spirv-std` & other deps.
fn build_deps(deps_target_dir: &Path, target: &str) -> TestDeps {
    // Build compiletests-deps-helper
    std::process::Command::new("cargo")
        .envs(
            spirv_builder::codegen_backend::rustup_toolchain_override()
                .map(|x| ("RUSTUP_TOOLCHAIN", x)),
        )
        .args([
            "build",
            "-p",
            "compiletests-deps-helper",
            "-Zbuild-std=core",
            "-Zbuild-std-features=compiler-builtins-mem",
            &*format!("--target={}", target_spec_json(target)),
        ])
        .arg("--target-dir")
        .arg(deps_target_dir)
        .env("RUSTFLAGS", SPIRV_BUILD_RUSTFLAGS.join(" "))
        .stderr(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .status()
        .and_then(map_status_to_result)
        .unwrap();

    let compiler_builtins = find_lib(
        deps_target_dir,
        "compiler_builtins",
        DepKind::SpirvLib,
        target,
    );
    let core = find_lib(deps_target_dir, "core", DepKind::SpirvLib, target);
    let spirv_std = find_lib(deps_target_dir, "spirv_std", DepKind::SpirvLib, target);
    let glam = find_lib(deps_target_dir, "glam", DepKind::SpirvLib, target);
    let spirv_std_macros = find_lib(
        deps_target_dir,
        "spirv_std_macros",
        DepKind::ProcMacro,
        target,
    );

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
        clean_deps(deps_target_dir);
        build_deps(deps_target_dir, target)
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

fn clean_deps(deps_target_dir: &Path) {
    std::process::Command::new("cargo")
        .arg("clean")
        .arg("--target-dir")
        .arg(deps_target_dir)
        .stderr(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .status()
        .and_then(map_status_to_result)
        .unwrap();
}

enum FindLibError {
    Missing,
    Duplicate,
}

/// Attempt find the rlib that matches `base`, if multiple rlibs are found then
/// a clean build is required and `Err(FindLibError::Duplicate)` is returned.
fn find_lib(
    deps_target_dir: &Path,
    base: impl AsRef<Path>,
    dep_kind: DepKind,
    target: &str,
) -> Result<PathBuf, FindLibError> {
    let base = base.as_ref();
    let (expected_prefix, expected_extension) = dep_kind.prefix_and_extension();
    let expected_name = format!("{}{}", expected_prefix, base.display());

    let dir = deps_target_dir.join(dep_kind.target_dir_suffix(target));

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
                .map_or(false, |ext| ext == expected_extension);

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

lazy_static::lazy_static! {
    /// The RUSTFLAGS passed to all SPIR-V builds.
    static ref SPIRV_BUILD_RUSTFLAGS: Vec<&'static str> = {
        let target_features = [
            "Int8",
            "Int16",
            "Int64",
            "Float64",
            // Only needed for `ui/arch/read_clock_khr.rs`.
            "ShaderClockKHR",
            "ext:SPV_KHR_shader_clock",
        ];

        spirv_builder::codegen_backend::base_rustflags()
            .chain([
                "-Cdebuginfo=2".into(),
                "-Cembed-bitcode=no".into(),
                format!("-Ctarget-feature=+{}", target_features.join(",+")).into(),
            ]).map(|s| match s {
                Cow::Borrowed(s) => s,
                Cow::Owned(s) => String::leak(s),
            })
            .collect::<Vec<_>>()
    };
}

/// Convenience function to map process failure to results in Rust.
fn map_status_to_result(status: std::process::ExitStatus) -> io::Result<()> {
    match status.success() {
        true => Ok(()),
        false => Err(io::Error::new(
            io::ErrorKind::Other,
            format!(
                "process terminated with non-zero code: {}",
                status.code().unwrap_or(0)
            ),
        )),
    }
}
