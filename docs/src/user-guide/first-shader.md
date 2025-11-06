# Your first shader

Configure your shader crate as a `"dylib"` type crate, and add `spirv-std` to its dependencies:

```toml
[lib]
crate-type = ["dylib"]

[dependencies]
spirv-std = { version = "0.9" }
```

Make sure your shader code uses the `no_std` attribute and makes the `spirv` attribute visible in the global scope. Then, you're ready to write your first shader. Here's a very simple fragment shader called `main_fs` as an example that outputs the color red:

```rust,norun
#![no_std]

use spirv_std::spirv;
use spirv_std::glam::{vec4, Vec4};

#[spirv(fragment)]
pub fn main_fs(output: &mut Vec4) {
    *output = vec4(1.0, 0.0, 0.0, 1.0);
}
```
