#[cfg(feature = "clap")]
mod clap {
    pub use crate::*;
    use clap::Parser;

    /// look at the output of this test to see what `--help` prints
    #[test]
    fn test_clap_help() {
        // any malformed clap declarations should panic within clap's parse
        match SpirvBuilder::try_parse_from(["", "--help"]) {
            Ok(_) => panic!("`--help` should make clap return an error"),
            Err(e) => {
                let e = e.to_string();
                println!("{}", e);
                // sanity check help
                assert!(e.contains("--target"));
            }
        };
    }
}

mod dylib_lookup {
    use crate::{find_rustc_codegen_spirv_in_paths, rustc_codegen_spirv_dylib_name};
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    struct TempDir(PathBuf);

    impl TempDir {
        fn new(test_name: &str) -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or(Duration::ZERO)
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "spirv-builder-{test_name}-{}-{unique}",
                std::process::id()
            ));
            fs::create_dir_all(&path).unwrap();
            Self(path)
        }

        fn path(&self) -> &Path {
            &self.0
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn finds_exact_backend_dylib() {
        let dir = TempDir::new("exact");
        let exact = dir.path().join(rustc_codegen_spirv_dylib_name());
        fs::File::create(&exact).expect("failed to create exact backend dylib file");

        assert_eq!(
            find_rustc_codegen_spirv_in_paths(vec![dir.path().to_path_buf()]),
            Some(exact)
        );
    }

    #[test]
    fn falls_back_to_hashed_backend_dylib() {
        let dir = TempDir::new("hashed");
        let hashed = dir.path().join(format!(
            "{}rustc_codegen_spirv-deadbeef{}",
            std::env::consts::DLL_PREFIX,
            std::env::consts::DLL_SUFFIX
        ));
        fs::File::create(&hashed).expect("failed to create hashed backend dylib file");

        assert_eq!(
            find_rustc_codegen_spirv_in_paths(vec![dir.path().to_path_buf()]),
            Some(hashed)
        );
    }

    #[test]
    fn prefers_exact_match_over_hashed_match_in_earlier_dir() {
        let hashed_dir = TempDir::new("hashed-first");
        let exact_dir = TempDir::new("exact-second");
        let hashed = hashed_dir.path().join(format!(
            "{}rustc_codegen_spirv-deadbeef{}",
            std::env::consts::DLL_PREFIX,
            std::env::consts::DLL_SUFFIX
        ));
        let exact = exact_dir.path().join(rustc_codegen_spirv_dylib_name());
        fs::File::create(&hashed).expect("failed to create hashed backend dylib file");
        fs::File::create(&exact).expect("failed to create exact backend dylib file");

        assert_eq!(
            find_rustc_codegen_spirv_in_paths(vec![
                hashed_dir.path().to_path_buf(),
                exact_dir.path().to_path_buf()
            ]),
            Some(exact)
        );
    }
}

mod rustflags {
    use crate::{Capability, EnvOverrides, ShaderPanicStrategy, SpirvBuilder, SpirvMetadata};
    use std::path::Path;

    const BACKEND: &str = "/backend/librustc_codegen_spirv.so";

    fn builder() -> SpirvBuilder {
        SpirvBuilder::new("/shader", "spirv-unknown-vulkan1.2")
    }

    fn rustflags(builder: &SpirvBuilder, env: &EnvOverrides) -> Vec<String> {
        builder.rustflags(Path::new(BACKEND), env)
    }

    /// A builder that emits no configurable flags at all (the default panic strategy does).
    fn silent_builder() -> SpirvBuilder {
        builder().shader_panic_strategy(ShaderPanicStrategy::SilentExit)
    }

    /// The flags after the fixed ones every build gets.
    fn extra_rustflags(builder: &SpirvBuilder, env: &EnvOverrides) -> Vec<String> {
        let fixed = rustflags(&silent_builder(), &EnvOverrides::default()).len();
        rustflags(builder, env)[fixed..].to_vec()
    }

    #[test]
    fn fixed_flags_start_with_the_backend() {
        let flags = rustflags(&builder(), &EnvOverrides::default());
        assert_eq!(flags[0], format!("-Zcodegen-backend={BACKEND}"));
        assert!(flags.contains(&"-Zbinary-dep-depinfo".to_string()));
        assert!(flags.contains(&"-Zshare-generics=off".to_string()));
    }

    #[test]
    fn silent_exit_has_no_llvm_args_or_target_features() {
        let flags = extra_rustflags(&silent_builder(), &EnvOverrides::default());
        assert_eq!(flags, Vec::<String>::new());
    }

    #[test]
    fn builder_options_map_to_flags() {
        let builder = builder()
            .multimodule(true)
            .spirv_metadata(SpirvMetadata::Full)
            .relax_block_layout(true)
            .preserve_bindings(true)
            .shader_panic_strategy(ShaderPanicStrategy::DebugPrintfThenExit {
                print_inputs: true,
                print_backtrace: false,
            })
            .capability(Capability::Int8)
            .extension("SPV_KHR_foo")
            .extra_arg("--dump-post-link=/tmp/x")
            .deny_warnings(true);
        assert_eq!(
            extra_rustflags(&builder, &EnvOverrides::default()),
            [
                "-Cllvm-args=--module-output=multiple --spirv-metadata=full \
                 --relax-block-layout --preserve-bindings \
                 --abort-strategy=debug-printf+inputs --dump-post-link=/tmp/x",
                "-Ctarget-feature=+ext:SPV_KHR_non_semantic_info,+Int8,+ext:SPV_KHR_foo",
                "-Dwarnings",
            ]
        );
    }

    #[test]
    fn warns_about_extra_args_replaced_by_env_codegen_args() {
        let env = EnvOverrides {
            codegen_args: Some("--from-env".into()),
            ..EnvOverrides::default()
        };
        let with_extra_args = silent_builder().extra_arg("--from-builder");
        assert_eq!(
            with_extra_args.ignored_extra_args_warning(&env).as_deref(),
            Some(
                "`RUSTGPU_CODEGEN_ARGS` is set, ignoring the codegen args set via \
                 `SpirvBuilder::extra_arg`: [\"--from-builder\"]"
            )
        );
        // nothing ignored
        assert_eq!(silent_builder().ignored_extra_args_warning(&env), None);
        assert_eq!(
            with_extra_args.ignored_extra_args_warning(&EnvOverrides::default()),
            None
        );
    }

    #[test]
    fn env_codegen_args_replace_extra_args_and_env_rustflags_are_appended() {
        let builder = silent_builder().extra_arg("--from-builder");
        let env = EnvOverrides {
            codegen_args: Some("--from-env  --and-more".into()),
            rustflags: Some("-Cfoo -Cbar".into()),
            cargoflags: None,
        };
        assert_eq!(
            extra_rustflags(&builder, &env),
            ["-Cllvm-args=--from-env --and-more", "-Cfoo", "-Cbar"]
        );
    }
}
