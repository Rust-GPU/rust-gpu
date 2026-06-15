# Writing shaders

In this chapter we will have a look how to make your own shaders in rust-gpu. 

This introduction is written based on [our rust-gpu template project](./template.md), we recommend setting it up locally if you want to follow along.



### Your first fragment shader

If you open up [`/mygraphics-shaders/src/lib.rs`](https://github.com/Rust-GPU/rust-gpu-template/blob/main/generated/graphics/wgpu/cargo-gpu/mygraphics-shaders/src/lib.rs), you'll find a function called `main_fs` like this:

```rust
#[spirv(fragment)]
pub fn main_fs(vtx_color: Vec3, output: &mut Vec4) {
    *output = Vec4::from((vtx_color, 1.));
}
```

This is an ordinary rust function with a special macro attached to it, marking it as a fragment shader (sometimes also called pixel shader). In this demo, it is evaluated once for each pixel of our triangle and computes the color that this particular pixel should be. So if we wanted our triangle to be all red, we could change the contents of the function to:

```rust
*output = Vec4::new(1., 0., 0., 1.);
```

Now `cargo run` the project and you should see a red triangle.

Just setting the triangle to a single color is boring, so let's have a look how to declare inputs and outputs.



### Inputs and Outputs

The fragment shader above is a great example for the two basic parameters:
* `vtx_color: Vec3` parameters passed **by-value** declare an **input** to the shader. In GLSL: `in vec3 vtx_color;`. 
* `output: &mut Vec4`: parameters passed **by-mutable-reference** declare an **output** to the shader. In GLSL: `out vec4 output;`.

Unlike most other shader languages, inputs and outputs can't just be declared anywhere within our code and includes. Instead, they are passed as parameters to your entry point function, centralizing where they can be declared. 

> [!Note]
> While this design makes it a lot easier to figure out all the things your shader is declaring, we are aware that it makes it difficult to share these declarations across multiple shaders. We hope to rectify these problems in future rust-gpu versions.



### Built-ins and Bindings

Inputs and outputs are by far not the only declaration, so let's have a look at the vertex shader:

```rust
#[spirv(vertex)]
pub fn main_vs(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] constants: &ShaderConstants,
    #[spirv(vertex_index)] vert_id: i32,
    #[spirv(position)] vtx_pos: &mut Vec4,
    vtx_color: &mut Vec3,
) {
    let time = constants.time * constants.speed + vert_id as f32 * (2. * PI * 120. / 360.);
    let position = vec2(f32::sin(time), f32::cos(time)) + constants.offset;
    *vtx_pos = Vec4::from((position, 0.0, 1.0));

    *vtx_color = [vec3(1., 0., 0.), vec3(0., 1., 0.), vec3(0., 0., 1.)][vert_id as usize % 3];
}
```

The vertex shader makes the triangle rotate by computing the vertex positions on the GPU.

It declares two built-ins that are only available vertex shader:
* `#[spirv(descriptor_set = 0, binding = 0, storage_buffer)] constants: &ShaderConstants` declares a storage buffer on the "slot" at descriptor set 0 and binding 0 in glsl terminology. This translates to bind group 0 and binding 0 in the wgpu API, we'll have more detailed look at buffers in the next section. GLSL: `layout(set = 0, binding = 0) ShaderConstants constants;`
* `#[spirv(vertex_index)] vert_id: i32` is an input built-in that is the index of the vertex. GLSL: `gl_VertexID`
* `#[spirv(position)] vtx_pos: &mut Vec4` is an output built-in to set the position of the vertex for the rasterizer. GLSL: `gl_Position`
* `vtx_color: &mut Vec3` is a standard output that is forwarded to our fragment shader. GLSL: `out vec3 vtx_color`

> [!Note]
> Unfortunately, the built-ins have magic names. You can look up most of them in [this compiletest](https://github.com/Rust-GPU/rust-gpu/blob/main/tests/compiletests/ui/spirv-attr/all-builtins.rs) and [in the compiler source code here](https://github.com/Rust-GPU/rust-gpu/blob/main/crates/rustc_codegen_spirv/src/symbols.rs#L38).



### Sharing data structures between CPU and shaders

The vertex shader declares a storage buffer of type `ShaderConstants`, which is defined in the shader crate like this:

```rust
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct ShaderConstants {
    pub time: f32,
    pub speed: f32,
    pub offset: Vec2,
}
```

It is just an ordinary rust struct that derives [`bytemuck::Pod`](https://docs.rs/bytemuck/latest/bytemuck/trait.Pod.html). Deriving `Pod` allows us to use the bytemuck library to convert our struct into a slice of u8s, write those bytes into a buffer on the GPU, and convert them back to our `ShaderConstants` in our shader. But to implement `Pod` our struct needs to fulfill a complex set of unsafe invariants, which the `Pod` derive macro will check for: 
* It must be marked with `#[repr(C)]` so our struct has a consistent layout between our CPU compile and shader compile.
* The type must not have any padding.
* It also needs to derive [`Zeroable`](https://docs.rs/bytemuck/latest/bytemuck/trait.Zeroable.html)
* And all of the above must also hold for any members of the struct.

> [!Warning]
> rust-gpu does not verify that your declarations actually implement `Pod`

Since the CPU crate `mygraphics` depends on our GPU crate `mygraphics-shaders`, we can use our `ShaderConstants` type in our CPU code. Here is an example constructing our `ShaderConstants`, converting them to a `&[u8]` and creating a new `wgpu` buffer with them:

```rust
let shader_constants = ShaderConstants {
    time: get_time(),
    speed: 0.2,
    offset: Vec2::new(0., 0.),
};
let contents = bytemuck::bytes_of(&shader_constants);
let buffer = device.create_buffer_init(&BufferInitDescriptor {
    label: Some("ShaderConstants"),
    usage: BufferUsages::STORAGE,
    contents,
});
```

You can then create a wgpu bind group / vulkan descriptor set with said buffer and submit some work to the GPU with our shader, not detailed here as we assume you to already be familiar with graphics APIs.
