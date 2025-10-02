# Examples

This directory contains several examples of rust-gpu use. There are shader
examples in `shaders`, and runner programs that build and execute the shaders
in a variety of ways in `runners`.

The shaders:
- **sky:** draws a landscape with a small white sun, blue sky, and yellow
  ground. This is the default shader.
- **simplest:** draws a red triangle on a green background.
- **mouse:** a swirling animation that can be influenced by clicking and
  dragging the mouse cursor.
- **compute:** a compute shader that prints a sequence of integers from the
  '3x+1' problem.

The runners:
- **WGPU:** runs the shader code on the GPU using [wgpu](https://wgpu.rs), a
  graphics crate based on WebGPU.
- **WGPU+wasm:** like WGPU but runs in a web browser using
  [wasm](https://webassembly.org/).
- **ash:** runs the shader code on the GPU using
  [ash](https://crates.io/crates/ash), a Vulkan wrapper crate for Rust.
- **CPU:** runs the shader code on the CPU, using rayon for parallelism.

Not all shaders work with all runners. The following combinations are
supported.

- WGPU runner:
  - `cargo run --release -p example-runner-wgpu` runs the sky shader.
  - `cargo run --release -p example-runner-wgpu -- --shader=sky` also runs the
    sky shader.
  - `cargo run --release -p example-runner-wgpu -- --shader=simplest` runs the
    simplest shader.
  - `cargo run --release -p example-runner-wgpu -- --shader=mouse` runs the
    mouse shader.
  - `cargo run --release -p example-runner-wgpu -- --shader=compute` runs the
    compute shader.

- WGPU+wasm runner in the browser (requires browser WebGPU support, most
  recently tested with Chromium 140 on Linux):
  - `rustup target add wasm32-unknown-unknown` installs the necessary wasm
    support for Rust.
  - `cargo run-wasm -p example-runner-wgpu` runs the local server hosting the
    mouse shader.
  - `chromium --enable-unsafe-webgpu http://localhost:8000` runs Chromium with
    WebGPU enabled and views the mouse shader. (The mouse shader is the default
    on WGPU+wasm.)

- ash runner:
  - MacOS: requires MoltenVK (or the Vulkan SDK) to be installed
  - `cargo run --release -p example-runner-ash` runs the sky shader. Use the up and
    down arrows to adjust the sun's intensity. Use F5 to recompile the shader
    code (but note that the image won't redraw afterwards unless the intensity is
    adjusted).
  - `cargo run --release -p example-runner-ash -- --shader=sky` also runs the sky shader.
  - `cargo run --release -p example-runner-ash -- --shader=simplest` runs the simplest shader.
  - `cargo run --release -p example-runner-ash -- --shader=mouse` runs the
    mouse shader. The click-and-drag functionality is missing and the image
    only animates on mouse or keyboard input, but it still is a useful example.
    A pull request to fix these shortcomings would be welcome!

- CPU runner:
  - `cargo run --release -p example-runner-cpu` runs the sky shader.

Finally, the `multibuilder` folder is a very short example of how to use the
`multimodule` feature of `spirv-builder`.
