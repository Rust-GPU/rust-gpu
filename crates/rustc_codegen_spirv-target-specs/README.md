# `rustc_codegen_spirv-target-specs`

The target spec json files of rust-gpu to hand to the rustc compiler, declaring various metadata about our codegen backend.

## Features
* `include_str`: include target specs as string constants, for bundling with `cargo-gpu`
* `dir_path`: export a path to the target specs dir, for `spirv-builder` and `compiletest` in this repo
