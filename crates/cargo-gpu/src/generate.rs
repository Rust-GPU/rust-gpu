use cargo_generate::{TemplatePath, Vcs};
use clap::Args;
use std::path::PathBuf;

/// Runs `cargo generate` on the rust-gpu template repository at <https://github.com/Rust-GPU/rust-gpu-template.git>
///
/// Some sensible params are copied from [`cargo_generate::GenerateArgs`], unnecessary ones are omitted. If you want
/// more options, just use `cargo generate` directly. Unfortunately, we can't `#[command(flatten)]` a
/// [`cargo_generate::GenerateArgs`] member. This prevents `cargo gpu new` from working on its own, as it's expecting
/// at least one parameter and instead prints help.
#[derive(Default, Debug, Clone, Args)]
#[command(arg_required_else_help(false))]
pub struct Generate {
    /// Git repository to clone template from.
    #[arg(
        short,
        long,
        group("SpecificPath"),
        default_value = "https://github.com/Rust-GPU/rust-gpu-template.git"
    )]
    pub git: Option<String>,
    /// Specify the VCS used to initialize the generated template.
    #[arg(long, value_parser)]
    pub vcs: Option<Vcs>,
    /// Define a value for use during template expansion. E.g `--define foo=bar`
    #[arg(long, short, number_of_values = 1, value_parser)]
    pub define: Vec<String>,
    /// Generate the template directly into the current dir. No subfolder will be created and no vcs
    /// is initialized.
    #[arg(long, action)]
    pub init: bool,
    /// Generate the template directly at the given path.
    #[arg(long, value_parser, value_name = "PATH")]
    pub destination: Option<PathBuf>,
    /// Allow the template to overwrite existing files in the destination.
    #[arg(short, long, action)]
    pub overwrite: bool,
}

impl Generate {
    pub fn run(self) -> anyhow::Result<()> {
        cargo_generate::generate(cargo_generate::GenerateArgs {
            template_path: TemplatePath {
                auto_path: Some("".to_owned()),
                git: self.git,
                ..TemplatePath::default()
            },
            vcs: self.vcs,
            define: self.define,
            init: self.init,
            destination: self.destination,
            overwrite: self.overwrite,
            // `cargo-generate` expects this to be a template for a crate, but we're generating an entire workspace,
            // so their "add to workspace" fails. This flag disables that.
            no_workspace: true,
            ..cargo_generate::GenerateArgs::default()
        })?;
        Ok(())
    }
}
