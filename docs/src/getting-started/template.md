# Generating a project

Here we'll describe how you can use our cmdline utility to generate a rust-gpu project from a template. On the next page we'll inspect its structure, so you can replicate it in any existing project.

First, install our `cargo gpu` cli, if you haven't already:

```shell
cargo install cargo-gpu
```

Then generate a rust-gpu project with:

```shell
cargo gpu generate
```

(This subcommand is merely a wrapper around [`cargo-generate`](https://crates.io/crates/cargo-generate/) invoked on our [`rust-gpu-template` repository](https://github.com/Rust-GPU/rust-gpu-template). If you don't want to run that command, you can also download any expanded template from [the generated subdirectory](https://github.com/Rust-GPU/rust-gpu-template/tree/main/generated).)

It will ask you a few questions on what kind of project you want:

**Which sub-template should be expanded?**

* **graphics**: renders a colorful triangle with vertex and fragment shaders

Feel free to PR new templates!

**What API?**

* **[wgpu](https://github.com/gfx-rs/wgpu)**: WebGPU with a nice rusty interface
* **[ash](https://github.com/ash-rs/ash)**: lightweight wrapper around Vulkan

If you're unsure, wgpu is generally easier to use.

**How to integrate rust-gpu?**

* **[cargo-gpu](https://github.com/Rust-GPU/cargo-gpu) (recommended)**: It's a rust-gpu installation manager that isolates the specific nightly toolchain that rust-gpu requires, allowing the rest of your project to remain on a stable toolchain (or any other toolchain). Other users don't need the cli installed, it just offers some extra utilities.
* **[spirv-builder](https://github.com/Rust-GPU/rust-gpu/tree/main/crates/spirv-builder)** is the older setup that requires your entire project to be compiled using the specific nightly rust-gpu toolchain.

It should then create a new project under the subdirectory of your choosing. Simply `cd` into it and `cargo run` should get you a rotating triangle!

It may take a while for `mygraphics(build)` to finish when you run it for the first time. We have to download the specific rust toolchain we need and build the rust-gpu codegen backend that functions with it. These preparations are cached externally, so even after a `cargo clean` the build should be significantly faster.
