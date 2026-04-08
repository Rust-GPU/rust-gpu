# cargo-gpu

`cargo-gpu` is an installation manager and command line tool for [rust-gpu](https://github.com/Rust-GPU/rust-gpu/). `cargo-gpu` is not an essential requirement, it should just make working with `rust-gpu` easier.

There are 2 ways to use it:
1. Through a CLI, ie `cargo gpu ...`
2. As a crate included in your build scripts or executables

## 1. CLI Quickstart

To install the command line tool, ensure you are using `rustup`. Then run:

```
cargo install --git https://github.com/rust-gpu/cargo-gpu cargo-gpu
```

You can then use `cargo gpu` to compile your shader crates or use any of the other commands you're used to:

```
cargo gpu build
cargo gpu check
cargo gpu clippy
```

### Example project

To create an example project from our [templates](https://github.com/Rust-GPU/rust-gpu-template), use the command below:
```
cargo install cargo-generate
cargo generate --git https://github.com/Rust-GPU/rust-gpu-template
# choose any template you want, then select cargo-gpu cmdline integration
# you may have to adjust the crate name
cargo gpu build -p mygraphics-shaders
```

This plain invocation will compile the crate in the current directory and place the compiled shaders in the current directory.

Use `cargo gpu help` to see more options :)

## 2. Crate Quickstart

Add `cargo-gpu-install` as a regular or build dependency to your project, and use it like this:

```rust,no_run
let shader_crate = PathBuf::from("./shaders");
let backend = cargo_gpu_install::Install::from_shader_crate(shader_crate.clone()).run()?;
let mut builder = backend.to_spirv_builder(shader_crate, "spirv-unknown-vulkan1.2");
// configure the builder...
let spv_result = builder.build()?;
```

For more detail, see the [readme of `cargo-gpu-install`](crates/cargo-gpu-install/README.md) or use any of our [templates](https://github.com/Rust-GPU/rust-gpu-template) as reference and choosing the `cargo-gpu` integration.

## How it works

Behind the scenes `cargo gpu` compiles a custom [codegen backend](https://doc.rust-lang.org/beta/unstable-book/compiler-flags/codegen-backend.html)
for `rustc` that allows emitting [SPIR-V](https://www.khronos.org/spir/) assembly, instead of the conventional LLVM assembly. SPIR-V is a dedicated
graphics language that is aimed to be open and portable so that it works with as many drivers and devices as possible.

With the custom codegen backend (`rustc_codegen_spirv`) `cargo gpu` then compiles the shader it is pointed to. However, because custom codegen backends
are currently [an unstable feature](https://github.com/rust-lang/rust/issues/77933), `cargo gpu` also needs to install a "nightly" version of Rust. In
the usage instructions the backend and nightly Rust version are referred to as "artefacts" and can be explicitly managed with the arguments to the
`install` subcommand.

> [!TIP]
> Whilst `cargo gpu` attempts to isolate shader compilation as much possible, if the shader crate is contained in a workspace then it's possible that
> the nightly version required by the shader is, ironically, older than the Rust/Cargo versions required by the workspace. Say for instance the
> workspace might use a newer `Cargo.lock` layout not supported by the pinned version of the shader crate's custom codegen backend. The solution to
> this is to either exclude the shader from the workspace, or upgrade the shader's `spirv-std` dependency to the latest.

## CLI Usage

All the following arguments for the `build` and `install` commands can also be set in the shader crate's `Cargo.toml`
file. In general usage that would be the recommended way to set config. See `crates/shader-crate-template/Cargo.toml`
for an example.

````
  Commands:
    install  Install rust-gpu compiler artifacts
    build    Compile a shader crate to SPIR-V
    show     Show some useful values
    help     Print this message or the help of the given subcommand(s)
  
  Options:
    -h, --help
            Print help
  
    -V, --version
            Print version


* Install
  Install rust-gpu compiler artifacts
  
  Usage: cargo-gpu install [OPTIONS]
  
  Options:
        --shader-crate <SHADER_CRATE>
            Directory containing the shader crate to compile
            
            [default: ./]
  
        --spirv-builder-source <SPIRV_BUILDER_SOURCE>
            Source of `spirv-builder` dependency Eg: "https://github.com/Rust-GPU/rust-gpu"
  
        --spirv-builder-version <SPIRV_BUILDER_VERSION>
            Version of `spirv-builder` dependency.
            * If `--spirv-builder-source` is not set, then this is assumed to be a crates.io semantic
              version such as "0.9.0".
            * If `--spirv-builder-source` is set, then this is assumed to be a Git "commitsh", such
              as a Git commit hash or a Git tag, therefore anything that `git checkout` can resolve.
  
        --rebuild-codegen
            Force `rustc_codegen_spirv` to be rebuilt
  
        --auto-install-rust-toolchain
            Assume "yes" to "Install Rust toolchain: [y/n]" prompt
  
        --no-clear-target
            Clear target dir of `rustc_codegen_spirv` build after a successful build, saves about 200MiB of disk space
  
        --force-overwrite-lockfiles-v4-to-v3
            There is a tricky situation where a shader crate that depends on workspace config can have
            a different `Cargo.lock` lockfile version from the the workspace's `Cargo.lock`. This can
            prevent builds when an old Rust toolchain doesn't recognise the newer lockfile version.
            
            The ideal way to resolve this would be to match the shader crate's toolchain with the
            workspace's toolchain. However, that is not always possible. Another solution is to
            `exclude = [...]` the problematic shader crate from the workspace. This also may not be a
            suitable solution if there are a number of shader crates all sharing similar config and
            you don't want to have to copy/paste and maintain that config across all the shaders.
            
            So a somewhat hacky workaround is to have `cargo gpu` overwrite lockfile versions. Enabling
            this flag will only come into effect if there are a mix of v3/v4 lockfiles. It will also
            only overwrite versions for the duration of a build. It will attempt to return the versions
            to their original values once the build is finished. However, of course, unexpected errors
            can occur and the overwritten values can remain. Hence why this behaviour is not enabled by
            default.
            
            This hack is possible because the change from v3 to v4 only involves a minor change to the
            way source URLs are encoded. See these PRs for more details:
              * <https://github.com/rust-lang/cargo/pull/12280>
              * <https://github.com/rust-lang/cargo/pull/14595>
  
    -h, --help
            Print help (see a summary with '-h')


* Build
  Compile a shader crate to SPIR-V
  
  Usage: cargo-gpu build [OPTIONS]
  
  Options:
        --shader-crate <SHADER_CRATE>
            Directory containing the shader crate to compile
            
            [default: ./]
  
        --spirv-builder-source <SPIRV_BUILDER_SOURCE>
            Source of `spirv-builder` dependency Eg: "https://github.com/Rust-GPU/rust-gpu"
  
        --spirv-builder-version <SPIRV_BUILDER_VERSION>
            Version of `spirv-builder` dependency.
            * If `--spirv-builder-source` is not set, then this is assumed to be a crates.io semantic
              version such as "0.9.0".
            * If `--spirv-builder-source` is set, then this is assumed to be a Git "commitsh", such
              as a Git commit hash or a Git tag, therefore anything that `git checkout` can resolve.
  
        --rebuild-codegen
            Force `rustc_codegen_spirv` to be rebuilt
  
        --auto-install-rust-toolchain
            Assume "yes" to "Install Rust toolchain: [y/n]" prompt
  
        --no-clear-target
            Clear target dir of `rustc_codegen_spirv` build after a successful build, saves about 200MiB of disk space
  
        --force-overwrite-lockfiles-v4-to-v3
            There is a tricky situation where a shader crate that depends on workspace config can have
            a different `Cargo.lock` lockfile version from the the workspace's `Cargo.lock`. This can
            prevent builds when an old Rust toolchain doesn't recognise the newer lockfile version.
            
            The ideal way to resolve this would be to match the shader crate's toolchain with the
            workspace's toolchain. However, that is not always possible. Another solution is to
            `exclude = [...]` the problematic shader crate from the workspace. This also may not be a
            suitable solution if there are a number of shader crates all sharing similar config and
            you don't want to have to copy/paste and maintain that config across all the shaders.
            
            So a somewhat hacky workaround is to have `cargo gpu` overwrite lockfile versions. Enabling
            this flag will only come into effect if there are a mix of v3/v4 lockfiles. It will also
            only overwrite versions for the duration of a build. It will attempt to return the versions
            to their original values once the build is finished. However, of course, unexpected errors
            can occur and the overwritten values can remain. Hence why this behaviour is not enabled by
            default.
            
            This hack is possible because the change from v3 to v4 only involves a minor change to the
            way source URLs are encoded. See these PRs for more details:
              * <https://github.com/rust-lang/cargo/pull/12280>
              * <https://github.com/rust-lang/cargo/pull/14595>
  
    -o, --output-dir <OUTPUT_DIR>
            Path to the output directory for the compiled shaders
            
            [default: ./]
  
    -w, --watch
            Watch the shader crate directory and automatically recompile on changes
  
        --debug
            Build in release. Defaults to true
  
        --target <TARGET>
            The target triple, eg. `spirv-unknown-vulkan1.2`
            
            [default: spirv-unknown-vulkan1.2]
  
        --no-default-features
            Set --default-features for the target shader crate
  
        --features <FEATURES>
            Set --features for the target shader crate
  
        --deny-warnings
            Deny any warnings, as they may never be printed when building within a build script. Defaults to false
  
        --multimodule
            Splits the resulting SPIR-V file into one module per entry point. This is useful in cases where ecosystem tooling has bugs around multiple entry points per module - having all entry points bundled into a single file is the preferred system
  
        --spirv-metadata <SPIRV_METADATA>
            Sets the level of metadata (primarily `OpName` and `OpLine`) included in the SPIR-V binary. Including metadata significantly increases binary size
            
            [default: none]
  
            Possible values:
            - none:           Strip all names and other debug information from SPIR-V output
            - name-variables: Only include `OpName`s for public interface variables (uniforms and the like), to allow shader reflection
            - full:           Include all `OpName`s for everything, and `OpLine`s. Significantly increases binary size
  
        --capabilities <CAPABILITIES>
            Adds a capability to the SPIR-V module. Checking if a capability is enabled in code can be done via `#[cfg(target_feature = "TheCapability")]`
  
        --extensions <EXTENSIONS>
            Adds an extension to the SPIR-V module. Checking if an extension is enabled in code can be done via `#[cfg(target_feature = "ext:the_extension")]`
  
        --relax-struct-store
            Record whether or not the validator should relax the rules on types for stores to structs.  When relaxed, it will allow a type mismatch as long as the types are structs with the same layout.  Two structs have the same layout if
            
            1) the members of the structs are either the same type or are structs with same layout, and
            
            2) the decorations that affect the memory layout are identical for both types.  Other decorations are not relevant.
  
        --relax-logical-pointer
            Records whether or not the validator should relax the rules on pointer usage in logical addressing mode.
            
            When relaxed, it will allow the following usage cases of pointers: 1) `OpVariable` allocating an object whose type is a pointer type 2) `OpReturnValue` returning a pointer value
  
        --relax-block-layout <RELAX_BLOCK_LAYOUT>
            Records whether the validator should use "relaxed" block layout rules. Relaxed layout rules are described by Vulkan extension `VK_KHR_relaxed_block_layout`, and they affect uniform blocks, storage blocks, and push constants.
            
            This is enabled by default when targeting Vulkan 1.1 or later. Relaxed layout is more permissive than the default rules in Vulkan 1.0.
            
            [default: false]
            [possible values: true, false]
  
        --uniform-buffer-standard-layout
            Records whether the validator should use standard block layout rules for uniform blocks
  
        --scalar-block-layout
            Records whether the validator should use "scalar" block layout rules. Scalar layout rules are more permissive than relaxed block layout.
            
            See Vulkan extnesion `VK_EXT_scalar_block_layout`.  The scalar alignment is defined as follows: - scalar alignment of a scalar is the scalar size - scalar alignment of a vector is the scalar alignment of its component - scalar alignment of a matrix is the scalar alignment of its component - scalar alignment of an array is the scalar alignment of its element - scalar alignment of a struct is the max scalar alignment among its members
            
            For a struct in Uniform, `StorageClass`, or `PushConstant`: - a member Offset must be a multiple of the member's scalar alignment - `ArrayStride` or `MatrixStride` must be a multiple of the array or matrix scalar alignment
  
        --skip-block-layout
            Records whether or not the validator should skip validating standard uniform/storage block layout
  
        --preserve-bindings
            Records whether all bindings within the module should be preserved
  
    -m, --manifest-file <MANIFEST_FILE>
            Renames the manifest.json file to the given name
            
            [default: manifest.json]
  
    -h, --help
            Print help (see a summary with '-h')


* Show
  Show some useful values
  
  Usage: cargo-gpu show <COMMAND>
  
  Commands:
    cache-directory  Displays the location of the cache directory
    spirv-source     The source location of spirv-std
    commitsh         The git commitsh of this cli tool
    capabilities     All the available SPIR-V capabilities that can be set with `--capabilities`
    help             Print this message or the help of the given subcommand(s)
  
  Options:
    -h, --help
            Print help


        * Cache-directory
          Displays the location of the cache directory
          
          Usage: cargo-gpu show cache-directory
          
          Options:
            -h, --help
                    Print help


        * Spirv-source
          The source location of spirv-std
          
          Usage: cargo-gpu show spirv-source [OPTIONS]
          
          Options:
                --shader-crate <SHADER_CRATE>
                    The location of the shader-crate to inspect to determine its spirv-std dependency
                    
                    [default: ./]
          
            -h, --help
                    Print help


        * Commitsh
          The git commitsh of this cli tool
          
          Usage: cargo-gpu show commitsh
          
          Options:
            -h, --help
                    Print help


        * Capabilities
          All the available SPIR-V capabilities that can be set with `--capabilities`
          
          Usage: cargo-gpu show capabilities
          
          Options:
            -h, --help
                    Print help
````
