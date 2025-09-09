# Shading Languages

A typical graphics & shader ecosystem consists out of the following parts, one feeding into the next:

* a custom C-like shader language (SL)
* a shader compiler to turn the shader language into an Intermediate Representation (IR) or Intermediary Language (IL)
* the custom shader IR is stored on disk and distributed, think of it like Java or C# bytecode
* the app passes the shader IR to the graphics API
* the graphics driver compiles the IR into machine code on the fly, typically via an LLVM stack

Listed here are the graphics API with the typical shading language used. Note that OpenGL and WebGPU don't have a shader compiler or shader IR and require you to send the source code directly to the graphics API.

| API    | Platforms      | Shader language | compiler                                                                                                                                                               | Shader IR     |
|--------|----------------|-----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------|
| Vulkan | cross-platform | glsl            | [shaderc / glslc](https://github.com/google/shaderc) or [glslangvalidator](https://github.com/KhronosGroup/glslang)                                                    | SPIRV         |
| OpenGL | cross-platform | glsl            | N/A                                                                                                                                                                    | N/A (glsl)    |
| DX11   | Windows        | hlsl            | [DXC](https://github.com/microsoft/DirectXShaderCompiler) or deprecated [FXC](https://github.com/microsoft/DirectXShaderCompiler/wiki/Porting-shaders-from-FXC-to-DXC) | DXIL, SPIRV*  |
| DX12   | Windows        | hlsl            | [DXC](https://github.com/microsoft/DirectXShaderCompiler)                                                                                                              | DXIL, SPIRV*  |
| Metal  | MacOS, iOS     | msl             | [MSL compiler (?)](https://developer.apple.com/documentation/metal/building-a-shader-library-by-precompiling-source-files)                                             | air, metallib |
| WebGPU | browser        | wgsl            | N/A                                                                                                                                                                    | N/A (wgsl)    |

*[DirectX is deprecating DXIL and switching to SPIRV](https://devblogs.microsoft.com/directx/directx-adopting-spir-v/)

### Examples of typical compute-only stacks

* OpenCL: OpenCL C++ kernel -> clang / LLVM -> SPIRV for OpenCL
* SYCL: just OpenCL embedded in regular C++
* Cuda: Cuda C++ -> clang / LLVM -> PTX

### TLDR

Every vendor invents their own ecosystem stack, with a custom shading language, compiler, IR and graphics API. The industry standard solution (until a few years ago) was to use preprocessor macros to adjust for the differences between the various input languages. Yes, seriously. As you can imagine, just relying on preprocessors is a quite fragile system, since you need to write your shaders in a way that the dumbest compiler can understand it.

Enter transpilers, to compile from one language and ecosystem to another.
