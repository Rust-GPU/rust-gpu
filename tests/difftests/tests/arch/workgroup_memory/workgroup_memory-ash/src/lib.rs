#![no_std]

// Include the shader content from workgroup_memory-rust to keep them in sync
// Note: We need to skip the #![no_std] from the included file
include!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../workgroup_memory-rust/src/shader.rs"
));
