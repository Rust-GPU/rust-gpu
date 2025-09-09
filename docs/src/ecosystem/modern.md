# Modern shader compilers

### [slang](https://github.com/shader-slang/slang)
Is the new shining star of shader compilers. Introduces a new HLSL-like shading language that is well-designed from the ground up and introduces advanced features like generics and automatic differentiation. Compiles down into basically everything you could ever ask for:
* HLSL (DX11 and DX12)
* SPIRV (Vulkan)
* GLSL (Vulkan)
* MSL (Metal)
* WGSL (WebGPU)
* CUDA
* CPU

### [rust-gpu](https://github.com/Rust-GPU/rust-gpu/)
This project, compiling rust to SPIRV shaders, that can then be transpiled further into other shading languages.
