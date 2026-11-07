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
    use crate::{
        SpirvBuilderError, find_rustc_codegen_spirv_in_paths, rustc_codegen_spirv_dylib_name,
    };
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
    fn finds_nothing_in_empty_or_missing_dirs() {
        let empty = TempDir::new("empty");
        let missing = empty.path().join("does-not-exist");

        assert_eq!(find_rustc_codegen_spirv_in_paths(vec![]), None);
        assert_eq!(
            find_rustc_codegen_spirv_in_paths(vec![empty.path().to_path_buf(), missing]),
            None
        );
    }

    #[test]
    fn ignores_unrelated_dylibs() {
        let dir = TempDir::new("unrelated");
        let unrelated = dir.path().join(format!(
            "{}some_other_backend{}",
            std::env::consts::DLL_PREFIX,
            std::env::consts::DLL_SUFFIX
        ));
        fs::File::create(&unrelated).expect("failed to create unrelated dylib file");

        assert_eq!(
            find_rustc_codegen_spirv_in_paths(vec![dir.path().to_path_buf()]),
            None
        );
    }

    /// A missing backend dylib is reported as an error (it used to `panic!`).
    #[test]
    fn not_found_error_names_the_dylib() {
        let err = SpirvBuilderError::RustcCodegenSpirvDylibNotFound {
            filename: rustc_codegen_spirv_dylib_name(),
        };
        assert!(err.to_string().contains(&rustc_codegen_spirv_dylib_name()));
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
