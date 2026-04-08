#![expect(clippy::pub_use, reason = "pub use for build scripts")]
#![expect(
    missing_docs,
    clippy::missing_docs_in_private_items,
    reason = "crate docs are cfg'ed out"
)]
#![cfg_attr(doc, doc = include_str!("../README.md"))]

pub mod install;
mod install_toolchain;
pub mod spirv_source;
pub mod test;

pub use spirv_builder;

/// Central function to write to the user.
#[macro_export]
#[cfg(feature = "tty")]
macro_rules! user_output {
    ($($args: tt)*) => { {
        #[allow(
            clippy::allow_attributes,
            clippy::useless_attribute,
            unused_imports,
            reason = "`std::io::Write` is only sometimes called??"
        )]
        use std::io::Write as _;

        #[expect(
            clippy::non_ascii_literal,
            reason = "CRAB GOOD. CRAB IMPORTANT."
        )]
        {
            print!("🦀 ");
        }
        print!($($args)*);
        std::io::stdout().flush().unwrap();
   } }
}

/// Central function to write to the user.
#[macro_export]
#[cfg(not(feature = "tty"))]
macro_rules! user_output {
    ($($args: tt)*) => {{}};
}

/// The central cache directory of cargo gpu
///
/// # Errors
/// may fail if we can't find the user home directory
#[inline]
#[cfg(not(any(feature = "test", test)))]
#[expect(clippy::cfg_not_test, reason = "tests use different cache_dir")]
pub fn cache_dir() -> anyhow::Result<std::path::PathBuf> {
    use anyhow::Context as _;
    Ok(directories::BaseDirs::new()
        .with_context(|| "could not find the user home directory")?
        .cache_dir()
        .join("rust-gpu"))
}

#[cfg(any(feature = "test", test))]
pub use test::test_cache_dir as cache_dir;

/// Returns a string suitable to use as a directory.
///
/// Created from the spirv-builder source dep and the rustc channel.
fn to_dirname(text: &str) -> String {
    text.replace(
        [std::path::MAIN_SEPARATOR, '\\', '/', '.', ':', '@', '='],
        "_",
    )
    .split(['{', '}', ' ', '\n', '"', '\''])
    .collect::<Vec<_>>()
    .concat()
}
