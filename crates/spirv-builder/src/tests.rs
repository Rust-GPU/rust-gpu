#[cfg(feature = "clap")]
mod clap {
    pub use crate::*;
    use clap::Parser;

    /// look at the output of this test to see what `--help` prints
    #[test]
    fn test_clap_help() {
        match SpirvBuilder::try_parse_from(["", "--help"]) {
            Ok(_) => panic!("help must fail to parse"),
            Err(e) => {
                let e = e.to_string();
                println!("{}", e);
                assert!(e.contains("--target"));
            }
        };
    }

    #[test]
    fn test_clap_target() {
        let env = SpirvTargetEnv::OpenGL_4_2;
        let builder = SpirvBuilder::try_parse_from(["", "--target", &env.target_triple()])
            .map_err(|e| e.to_string())
            .unwrap();
        assert_eq!(builder.target, Some(env));
    }
}
