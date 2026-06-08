# Getting Started

Rust-GPU is a shader compiler that compiles Rust source code to SPIR-V, am intermediary representation (IR) for shaders.
Think of SPIR-V like java bytecode for shaders, that just represents the source code, and the driver will compile into the final machine code for the particular GPU you have on the fly. 
SPIR-V can be directly fed to the Vulkan API, to wgpu or can be transpiled into other shader IRs through the many transpiler available. We offer direct transpilation to wgsl for use with webgpu.

Setting up rust-gpu isn't as trivial as adding a cargo dependency. 

* [rust-gpu-template](https://github.com/Rust-GPU/rust-gpu-template)
