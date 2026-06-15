# Workspace structure

In [the previous chapter](./template.md), you've generated a new rust-gpu project. Inspecting the directory structure, you'll find something like this:

```shell
$ tree
.
├── Cargo.lock
├── Cargo.toml
├── mygraphics
│   ├── build.rs
│   ├── Cargo.toml
│   └── src
│       ├── lib.rs
│       ├── main.rs
│       └── wgpu_renderer
│           ├── mod.rs
│           ├── renderer.rs
│           ├── render_pipeline.rs
│           └── swapchain.rs
└── mygraphics-shaders
    ├── Cargo.toml
    └── src
        └── lib.rs
```

> [!Note]
> This is written against the graphics template with wgpu API and cargo-gpu, though other templates shouldn't differ much.

The first thing you'll notice that it's not just a single crate, but an entire workspace with multiple crates. Refer to [cargo's documentation on workspaces](https://doc.rust-lang.org/book/ch14-03-cargo-workspaces.html) if you've never worked with one. You can also see the two subdirectories `mygraphics` and `mygraphics-shaders`, which are the two crates in our workspaces.

## `mygraphics-shaders` crate

By the `shaders` suffix you can already guess this is the crate the shaders live in. 

Opening it's [`lib.rs`](https://github.com/Rust-GPU/rust-gpu-template/blob/main/generated/graphics/wgpu/cargo-gpu/mygraphics-shaders/src/lib.rs), you'll notice the many different `#[spirv(..)]` annotations on functions and function arguments. We'll discuss how shaders are written in the [next chapter](./shaders.md), so ignore all of these for now and focus on the declaration in the very first line: `#![no_std]`. 

This crate is a `no_std` crate, meaning you won't have access to the full standard library, but only a platform-independent subset that's called `core`. Meaning you won't be able to do file IO, send network packages or create threads. On a GPU, you won't have access to these APIs, as the GPU has no operating system to handle these requests for us. But you also won't be able to access `alloc`, loosing you the `Vec`, `HashMap` and `String` data structures, since (most) GPU APIs won't offer you a memory allocator either. (Some APIs do give you a built-in memory allocator, like CUDA or HIP, but we're not targeting those.) Feel free to read up on [rust's `no_std` documentation](https://docs.rust-embedded.org/book/intro/no-std.html) for more details.

The crate also has two dependencies:
* **[`spirv-std`](https://crates.io/crates/spirv-std)** is our standard library for GPU functionality. Here you'll find the `#[spirv(..)]` annotation as well as various intrinsics you can call to invoke special GPU functionality. More on that in the [next chapter](./shaders.md).
* **[`glam`](https://crates.io/crates/glam)** is a simple and fast vector library, providing you with types such as `Vec2`, `Vec3`, `Vec4` and many more. It even implements some swizzling with `Vec2::xyxx()` and `Vec2::from((vec2, 0., 1.))`.

Note that the [dependencies are declared like this](https://github.com/Rust-GPU/rust-gpu-template/blob/main/generated/graphics/wgpu/cargo-gpu/Cargo.toml): 
```toml
spirv-std = { version = "0.10.0" }
glam = { version = "0.33", default-features = false }
```

> [!Warning]
> The glam crate must have its default features disabled, since by default, it includes the `std` feature which doesn't work on GPUs. Our `spirv-std` crate will implicitly enable the `std` feature of `glam` if you're on a CPU, and will use the fallback `libm` feature when you're on the GPU. 

Additionally, the version of glam you use must also be compatible with `spirv-std`. We do support a number of older glam versions in case you're using a library that hasn't updated, but you must enable it as a feature. For example, if you want to use `glam` v0.32, replace the above dependencies with this:
```toml
spirv-std = { version = "0.10.0", default-features = false, features = ["glam_0_32"] }
glam = { version = "0.32", default-features = false }
```
As of writing this, we support `glam` v0.30 to v0.33, and expect that support to grow. Note that we do not support multiple glam versions in parallel, you must only enable one version.

## `mygraphics` crate

The `mygraphics` crate is the main executable and run on the CPU. Here you'll also have full access to `std` as you're used to, and the place where you interact with whatever graphics API you've chosen. The example implementation we provide are intentionally kept small, just enough to run the demo. Since this isn't a tutorial on how to use any particular graphics API, please refer to whatever documentation or tutorials exist for the graphics API you want to use.

## Building the shader

Within the build script we turn the shaders you've written in Rust into artifacts these graphics APIs can understand. A build script is run before the crate is compiled, allowing you to create arbitrary files and rust source code that your crate may need, see [cargo's documentation on build scripts](https://doc.rust-lang.org/cargo/reference/build-scripts.html) for more detail.

Dependencies of build scripts are separate from your crate's dependencies, so we need to declare those in the [`mygraphics` crate's `Cargo.toml`](https://github.com/Rust-GPU/rust-gpu-template/blob/main/generated/graphics/wgpu/cargo-gpu/mygraphics/Cargo.toml). `cargo-gpu-install` is a slimmed down version of `cargo-gpu` cli specifically for build scripts, with `anyhow` for error throwing convenience.
```toml
[build-dependencies]
cargo-gpu-install.workspace = true
anyhow.workspace = true
```

You'll find the script in [`./mygraphics/build.rs`](https://github.com/Rust-GPU/rust-gpu-template/blob/main/generated/graphics/wgpu/cargo-gpu/mygraphics/build.rs) and is arguably the most complicated part of the setup. If you're setting up a new project and not using a template, I'd recommend copying it and adjusting it as needed. The most important part to adjust is the path to the shader crate you want to build. Easiest is to locate it relative to the crate's directory with [`CARGO_MANIFEST_DIR`](https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-build-scripts).
```rust,ignore
let manifest_dir = env!("CARGO_MANIFEST_DIR");
let crate_path = [manifest_dir, "..", "mygraphics-shaders"]
    .iter()
    .copied()
    .collect::<PathBuf>();
```

The next step is to install the required toolchain and build the rust-gpu codegen backend. Instead of specifying a version to install, you pass in the path to the shader crate. It'll resolve the version to install from the `spirv-std` dependency, since those two versions must always *exactly* match.
```rust,ignore
let install = Install::from_shader_crate(crate_path.clone())
    .within_build_script()
    .run()?;
```

From the returned `install` object you can create arbitrary many `SpirvBuilder`s and configure them. Note that the target to compile for in this example is `spirv-unknown-vulkan1.3`, we'll talk a bit more about targets in a bit. Feel free to read up on [`SpirvBuilder`](https://docs.rs/spirv-builder/latest/spirv_builder/struct.SpirvBuilder.html)s different configuration properties for what all these toggles actually do.

> [!Warning]
> Within build scripts you must to enable the `build_script.defaults` property, so cargo knows when to rerun the build script, otherwise you might end up with outdated shader artifacts.

```rust,ignore
let mut builder = install.to_spirv_builder(crate_path, "spirv-unknown-vulkan1.3");
builder.build_script.defaults = true;
builder.spirv_metadata = SpirvMetadata::Full;
let compile_result = builder.build()?;
```

With the shader artifacts built, we need a way to give that artifact to our binary crate. There's many different approaches, from json metadata to rust codegen, but by far the easiest is to pass the path to the artifact as an enviromnent variable. 
```rust,ignore
let spv_path = compile_result.module.unwrap_single();
println!("cargo::rustc-env=SHADER_SPV_PATH={}", spv_path.display());
```

To include the artifact as a binary blob into your crate, it is as simple as
```rust,ignore
const SHADER_ARTIFACT: &[u8] = include_bytes!(env!("SHADER_SPV_PATH"));
```

## Integrating common graphics APIs

### wgpu

Recommended target: `spirv-unknown-naga-wgsl`

This target will produce a `*.wgsl` file containing wgsl source code. Internally, we compile to SPIR-V and then use [wgpu's `naga`](https://github.com/gfx-rs/wgpu/tree/trunk/naga) to transpile it to wgsl. Use it like this:
```rust,ignore
const WGSL: &str = include_str!(env!("SHADER_SPV_PATH"));
let module: wgpu::ShaderModule = device.create_shader_module(WGSL);
```

### ash / Vulkan

Recommended target: `spirv-unknown-vulkan1.1` or higher

This target will produce a `*.spv` file containing SPIR-V binary, with the [`ash`](https://crates.io/crates/ash) crate use it like this:
```rust,ignore
const SPV_BYTES: &[u8] = include_bytes!(env!("SHADER_SPV_PATH"));
let spv: Vec<u32> = ash::util::read_spv(&mut std::io::Cursor::new(SPV_BYTES))?;
let module: ash::vk::ShaderModule = device.create_shader_module(
    &vk::ShaderModuleCreateInfo::default().code(&spv),
    None,
)?;
```
