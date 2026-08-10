# `spirv-std-nightly`

Like [`spirv-std`](https://crates.io/crates/spirv-std), exposes the "standard library" for [rust-gpu](https://github.com/rust-gpu/rust-gpu#readme) SPIR-V shaders, but replaces most functions to use the nightly-only [`adt_const_params`](https://doc.rust-lang.org/beta/unstable-book/language-features/adt-const-params.html) feature. 

Since rust-gpu requires you to use a specific nightly anyway, this isn't much of an issue for crates that are only used within shaders. However, it may cause trouble in setups where crates are shared between GPU and CPU and the CPU side compiled with stable rustc, so const generics have been moved to this separate crate.
