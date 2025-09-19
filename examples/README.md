# Examples

This directory contains several examples of rust-gpu use. There are shader
examples in `shaders`, and runner programs that build and execute the shaders
in a variety of ways in `runners`.

The shaders:
- **sky:** draws a landscape with a small white sun, blue sky, and yellow
  ground. This is the default shader.
- **simplest:** draws a red triangle on a green background.
- **mouse:** a swirling animation that can be influenced by the mouse cursor.
- **compute:** a compute shader that prints a sequence of integers from the
  '3x+1' problem.

The runners:
- **WGPU:** runs the shader code on the GPU using [wgpu](https://wgpu.rs), a
  graphics crate based on WebGPU.
- **ash:** runs the shader code on the GPU using
  [ash](https://crates.io/crates/ash), a Vulkan wrapper crate for Rust.
- **CPU:** runs the shader code directly on the CPU.

Not all shaders work with all runners. The following combinations are
supported.

- WGPU runner:
  - `cargo run --bin example-runner-wgpu` runs the sky shader.
  - `cargo run --bin example-runner-wgpu -- --shader=sky` also runs the sky
    shader.
  - `cargo run --bin example-runner-wgpu -- --shader=simplest` runs the
    simplest shader.
  - `cargo run --bin example-runner-wgpu -- --shader=mouse` runs the mouse
    shader.
  - `cargo run --bin example-runner-wgpu -- --shader=compute` runs the compute
    shader.

- ash runner:
  - `cargo run --bin example-runner-ash` runs the sky shader. Use the up and
    down arrows to adjust the sun's intensity. Use F5 to recompile the shader
    code (but note that the image won't redraw afterwards unless the intensity is
    adjusted).

- CPU runner:
  - `cargo run --bin example-runner-cpu` runs the sky shader.

Finally, the `multibuilder` folder is a very short example of how to use the
`multimodule` feature of `spirv-builder`.
