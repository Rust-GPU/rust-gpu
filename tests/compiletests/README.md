# Compiletests

This folder contains tests known as "compiletests", where each file in the `ui` folder corresponds
to a single compiletest. Keep in mind that the tests here are not executed, they are merely checked
for compile errors and the resulting binaries being valid (with spirv-val). If you also want to run
your shaders and verify their output, have a look at the neighbouring "difftests" instead.

You can run compiletests via `cargo compiletest`, with an alias setup in `.cargo/config` for
`cargo run --release -p compiletests --`. You can filter to run specific tests by passing the
(partial) filenames like `cargo compiletest arch/subgroup`. You can also `--bless` the tests,
which will update their expected stderr output in their associated `.stderr` file, if you 
promise to **manually verify** the new contents before committing them. 

Our compiletests use the [`compiletest_rs`](https://github.com/Manishearth/compiletest-rs) library,
which is the compiletest framework within rustc itself, and some glue code in `src`.

## How to write a compiletest

Create a new `test.rs` file within `./ui` and copy in the following minimal contents:
```rust
// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {}
```

The first lines are meta-comments containing instructions on how this test should be compiled, at 
the very least `build-pass` or `build-fail`. Additional flags can be found in the next chapter. 
Then you declare your shader entry point as you would normally, here a simple fragment shader that
does nothing. You can only use external `spirv_std` crate as well as it's reexported dependents, so
to get a `Vec4` you can `use spirv_std::glam::Vec4` and if you need complex f32 operations you can 
`use spirv_std::num_traits::Float`, just like you would in a normal shader. 

The stderr output of the build operation is expected to match the contents of the file 
`path/to/test.rs.stderr` and may be updated with `--bless`. The stderr output may either come from
some errors emitted by rustc or the rust-gpu itself, usually on tests which are expected to fail, 
or from other flags to disassemble the shader. Please manually verify any changes to the output
before committing them.

## Meta-comments reference

Most meta-comments can be added multiple times, for example multiple `compile-flags` will be 
appended to each other. Listed here are some of the common ones seen in rust-gpu compile tests, a 
full reference can be found in the 
[Compiletest documentation of rustc](https://rustc-dev-guide.rust-lang.org/tests/ui.html#error-annotations).

* `// build-pass` the shader must build successfully
* `// build-fail` the shader must fail to build
* `// only-vulkan1.2` only run the test on target vulkan1.2, skip otherwise. Other targets are accepted. 
* `// ignore-vulkan1.2` skip the test on target vulkan1.2. Other targets are accepted.
* `// compile-flags: something` adds `something` to the compile flags 
* `// compile-flags: -C target-feature=+StorageImageExtendedFormats` add a `Capability` like with `SpirvBuilder.capabilities`
* `// compile-flags: -C +ext:SPV_KHR_ray_tracing` add an `Extension` like with `SpirvBuilder.extensions`
* `// compile-flags: -C target-feature=+MeshShadingEXT,+ext:SPV_EXT_mesh_shader` add both an extension and a capability, as they are often go together
* `// compile-flags: -C llvm-args=--abort-strategy=debug-printf` an example how to set some other property, see source of `SpirvBuilder` for reference

### Disassembly

All disassembly will be dumped to stderr, which makes the testing framework compare it to the 
contents of `path/to/test.rs.stderr`.

* `// compile-flags: -C llvm-args=--disassemble` disassemble the entire shader
* `// compile-flags: -C llvm-args=--disassemble-globals` disassemble only globals and function declarations, excludes function contents
* `// compile-flags: -C llvm-args=--disassemble-entry=main` disassembles the `main` entry point, does not work on functions
* `// compile-flags: -C llvm-args=--disassemble-fn=add_two_ints::add_two_ints` disassembles the 
  function `add_two_ints`. The name must be `<file_name>::<function_name>`, the function must 
  **not** be an entry point but must called by an entry point to not be dead-code eliminated.
  Usually, a test has the disassembled function and an entry point calling it.
* `// normalize-stderr-test "targetSpecificMsg" -> ""` Replaces any substrings in stderr with another to normalise output
  between different machines and targets. By default, you should have to not specify any and only add them as needed. 
  List of common substitutions:
  * remove debug info:
    * `// normalize-stderr-test "OpLine .*\n" -> ""` remove all line numbers
    * `// normalize-stderr-test "OpSource .*\n" -> ""` remove all source code
    * `// normalize-stderr-test "%\d+ = OpString .*\n" -> ""` remove all src file paths
  * when disassembling globals and testing on both `vulkan` and `spv` targets:
    * `// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""` remove the vulkan memory model *capability*, only used by `vulkan` targets
    * `// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""` remove the vulkan memory model *extension*, only used by `vulkan` targets
    * `// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"` replace the `Vulkan` memory model with `Simple`, which `spv` targets use
    * `// normalize-stderr-test "; .*\n" -> ""` remove comments on spirv version by rspirv
  * remove rustc error src paths:
    * `// normalize-stderr-test "\S*/lib/rustlib/" -> "$$SYSROOT/lib/rustlib/"` normalize path to crates delivered with rustc, such as `core`
    * `// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"` normalize path to the `spirv-std` crate
