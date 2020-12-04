initSidebarItems({"fn":[["memcmp","libcore requires a few external symbols to be defined: https://github.com/rust-lang/rust/blob/c2bc344eb23d8c1d18e803b3f1e631cf99926fbb/library/core/src/lib.rs#L23-L27 TODO: This is copied from `compiler_builtins/mem.rs`. Can we use that one instead? The note in the above link says \"[the symbols] can also be provided by the compiler-builtins crate\". The memcpy in `compiler_builtins` is behind a \"mem\" feature flag - can we enable that somehow? https://github.com/rust-lang/compiler-builtins/blob/eff506cd49b637f1ab5931625a33cef7e91fbbf6/src/mem.rs#L12-L13"]],"mod":[["storage_class","Storage ClassesClass of storage for declared variables. These types act as pointers to values either contained in the GPU's memory. For example; `Input<f32>` points to a `f32` that was provided as input from the pipeline, and `Private<f32>` points to a `f32` in the GPU's global memory. Intermediate values do not form a storage class, and unless stated otherwise, storage class-based restrictions are not restrictions on intermediate objects and their types."]],"struct":[["Image2d",""],["Image2dArray",""],["SampledImage",""],["Sampler",""]],"trait":[["Derivative",""]]});