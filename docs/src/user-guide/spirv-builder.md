# spirv-builder

If you're writing a bigger application and you want to integrate SPIR-V shader
crates to display, it's recommended to use `spirv-builder` in a build script.

1. Copy the [`rust-toolchain.toml`] file to your project. (You must use the same
   version of Rust as `rust-gpu`. Ultimately, the build will fail with a nice
   error message when you don't use the exact same version)
2. Reference `spirv-builder` in your Cargo.toml:
    ```toml
    [build-dependencies]
    spirv-builder = "0.9"
    ```
   All dependent crates are published on [crates.io](https://crates.io).
3. Create a `build.rs` in your project root.

#### `build.rs`
Paste the following into `build.rs`

```rust,no_run
use spirv_builder::{MetadataPrintout, SpirvBuilder};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    SpirvBuilder::new(shader_crate, target)
        .print_metadata(MetadataPrintout::Full)
        .build()?;
    Ok(())
}
```

Substituting `shader_crate` with a relative path to your shader crate. The values available for the `target` parameter are available
[here](./platform-support.md).  For example, if building for vulkan 1.1, use
`"spirv-unknown-vulkan1.1"`.

The `SpirvBuilder` struct has numerous configuration options available, see
[documentation](https://rust-gpu.github.io/rust-gpu/api/spirv_builder/struct.SpirvBuilder.html).

#### `main.rs`
The following will directly include the shader module binary into your application.
```rust,no_run
const SHADER: &[u8] = include_bytes!(env!("<shader_crate>.spv"));
```

> **Note** If your shader name contains hyphens, the name of environment variable will be the name with hyphens changed to underscores.

Keep in mind that by default, build-dependencies are built in debug mode. This
means that the rust-gpu compiler (`rustc_codegen_spirv`) will be built in debug
mode, and will be *incredibly* slow. You can solve this by placing this bit of
configuration in your workspace `Cargo.toml`:

```toml
# Compile build-dependencies in release mode with
# the same settings as regular dependencies.
[profile.release.build-override]
opt-level = 3
codegen-units = 16
[profile.dev.build-override]
opt-level = 3
```

Keep in mind this will optimize *all* build script dependencies as release,
which may slow down full rebuilds a bit. Please read [this
issue](https://github.com/EmbarkStudios/rust-gpu/issues/448) for more
information, there's a few important caveats to know about this.
