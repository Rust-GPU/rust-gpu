# Introduction

Welcome to the Rust-GPU dev guide! This documentation is meant for documenting
how to use and develop on Rust-GPU.

> [!Important]
> This introduction assumes a basic understanding of GPU and graphics programming. I highly recommend following a tutorial until you have least a colorful triangle on screen or ran your first few compute shaders.
> 
> Some of my recommendations:
> * [learn wgpu](https://sotrh.github.io/learn-wgpu/) teaches you the modern WebGPU API with the [wgpu](https://github.com/gfx-rs/wgpu) crate, but doesn't go much beyond the basics.
> * [learnopengl.com](https://learnopengl.com/) is the best graphics programming guide to this day, but it teaches the older OpenGL API which is not supported by rust-gpu. However, it is one of the best sources intermediary rendering techniques, which you should be able to transfer to a more modern graphics API, or use [glutin](https://crates.io/crates/glutin) for Rust OpenGL bindings. 
> * [How to Vulkan](https://www.howtovulkan.com/) is a tutorial for modern Vulkan and highly recommended over all older Vulkan tutorials. I would not recommend Vulkan for a beginner, since it's a quite extensive API, but it has the most features, extensions and is the best documented API. Unfortunately, rust-gpu doesn't support Buffer Device Address (BDA) yet, so you'll have to resort to ordinary descriptor sets or arrays of descriptor sets like you'd see for bindless textures.
