# Images

Common image formats can be found in the [`spirv_std::image`](https://rust-gpu.github.io/rust-gpu/api/spirv_std/image/index.html) module. For example, `Image2d` is a very commonly used type, corresponding to `texture2D` in GLSL, and is likely what you want if you want a regular old sampled texture. Here's an example for sampling a texture, though you'd also find many other functions in the [docs on `Image`](https://docs.rs/spirv-std/latest/spirv_std/image/struct.Image.html):

```rust,no_run
#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0)] image2d: &Image2dI,
    #[spirv(descriptor_set = 1, binding = 1)] image2d_array: &Image2dArray,
    #[spirv(descriptor_set = 2, binding = 2)] cubemap: &Cubemap,
    #[spirv(descriptor_set = 3, binding = 3)] sampler: &Sampler,
) {
    let v2 = Vec2::new(0.0, 1.0);
    let v3 = Vec3::new(0.0, 1.0, 2.0);
    let r1: IVec4 = image2d.sample(*sampler, v2);
    let r2: Vec4 = image2d_array.sample(*sampler, v3);
    let r3: Vec4 = cubemap.sample(*sampler, v3);
}
```

Use storage images to write to images:

```rust
#[spirv(fragment)]
pub fn main(#[spirv(descriptor_set = 0, binding = 0)] storage_image: &StorageImage2d) {
    let pixel = UVec2::new(123, 456);
    let color = Vec4::new(0.3, 0.4, 0.5, 0.6);
    unsafe {
        storage_image.write(pixel, color);
    }
}
```

There are a huge number of combinations of image types in SPIR-V, which are represented by a const
generic type called `spirv_std::image::Image`. Specifying the generic parameters of this
type is incredibly tedious, so we have a wrapper macro `spirv_std::Image!` to write the type
instead. The types you've used above are simply type aliases for the output of that macro:

```rust,no_run
type Image2d = Image!(2D, type=f32, sampled);
type Image2dArray = crate::Image!(2D, type=f32, sampled, arrayed);
type Cubemap = crate::Image!(cube, type=f32, sampled);
```

See the [docs of the `Image!` macro](https://rust-gpu.github.io/rust-gpu/api/spirv_std/image/macro.Image.html) for it's syntax.
