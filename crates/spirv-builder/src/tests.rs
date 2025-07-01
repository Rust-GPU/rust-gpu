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
