//! Display various information about `cargo gpu`, eg its cache directory.

use crate::cache_dir;
use crate::spirv_builder::Capability;
use crate::spirv_source::SpirvSource;

/// Show the computed source of the spirv-std dependency.
#[derive(Clone, Debug, clap::Parser)]
pub struct SpirvSourceDep {
    /// The location of the shader-crate to inspect to determine its spirv-std dependency.
    #[clap(long, default_value = "./")]
    pub shader_crate: std::path::PathBuf,
}

/// Different tidbits of information that can be queried at the command line.
#[derive(Clone, Debug, clap::Subcommand)]
pub enum Info {
    /// Displays the location of the cache directory
    CacheDirectory,
    /// The source location of spirv-std
    SpirvSource(SpirvSourceDep),
    /// The git commitsh of this cli tool.
    Commitsh,
    /// All the available SPIR-V capabilities that can be set with `--capabilities`
    Capabilities,
}

/// `cargo gpu show`
#[derive(clap::Parser)]
pub struct Show {
    /// Display information about rust-gpu
    #[clap(subcommand)]
    command: Info,
}

impl Show {
    /// Entrypoint
    pub fn run(&self) -> anyhow::Result<()> {
        log::info!("{:?}: ", self.command);

        #[expect(
            clippy::print_stdout,
            reason = "The output of this command could potentially be used in a script, \
                      so we _don't_ want to use `crate::user_output`, as that prefixes a crab."
        )]
        match &self.command {
            Info::CacheDirectory => {
                println!("{}\n", cache_dir()?.display());
            }
            Info::SpirvSource(SpirvSourceDep { shader_crate }) => {
                let rust_gpu_source = SpirvSource::get_rust_gpu_deps_from_shader(shader_crate)?;
                println!("{rust_gpu_source}\n");
            }
            Info::Commitsh => {
                println!("{}", env!("GIT_HASH"));
            }
            Info::Capabilities => {
                println!("All available options to the `cargo gpu build --capabilities` argument:");
                #[expect(
                    clippy::use_debug,
                    reason = "It's easier to just use `Debug` formatting than implementing `Display`"
                )]
                for capability in Self::capability_variants_iter() {
                    println!("  {capability:?}");
                }
            }
        }

        Ok(())
    }

    /// Iterator over all `Capability` variants.
    fn capability_variants_iter() -> impl Iterator<Item = Capability> {
        // Since spirv::Capability is repr(u32) we can iterate over
        // u32s until some maximum
        #[expect(clippy::as_conversions, reason = "We know all variants are repr(u32)")]
        let last_capability = Capability::CacheControlsINTEL as u32;
        (0..=last_capability).filter_map(Capability::from_u32)
    }
}
