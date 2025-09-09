# Transpilers

Transpilers convert from one shading language / graphics ecosystem to another. 

### [spirv-cross](https://github.com/KhronosGroup/SPIRV-Cross)
Inputs: SPIRV
Outputs: GLSL, HLSL (DirectX), MSL (Metal)
Special: JSON output for reflection

One of the first transpiler to exist, converts SPIRV to various other formats. Has a bunch of options to tweak the output as well to adjust for differences between the APIs, since some APIs may expose a feature another API does not. The first time you could write everything in glsl and just transpile it afterward.

### [naga](https://github.com/gfx-rs/wgpu/tree/trunk/naga)
Inputs: SPIRV, wgsl
Outputs: SPIRV, HLSL (DirectX), MSL (Metal), GLSL

Naga is the transpiler of [wgpu](https://github.com/gfx-rs/wgpu), written in Rust. It's main target is to accept the wgsl submitted via the WebGPU API and turn it into the many different shading languages of the native platforms. Notably, is build for adding safety to shader languages and can add bounds checks and the like.