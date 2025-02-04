# Testing Rust-GPU

Rust-GPU has a couple of different kinds of tests, most can be ran through
`cargo test`. Rust-GPU also has end-to-end tests for compiling Rust and
validating its SPIR-V output, which can ran by running `cargo compiletest`. Finally,
Rust-GPU has differential tests, which runs Rust and WGSL shaders and
makes sure they have the same output. These can be run with `cargo difftest`.

```bash
cargo test && cargo compiletest && cargo difftest
```

## Compile Tests

### Adding Tests

Rust-GPU's end-to-end test's use an external version of the [`compiletest`] tool
as a testing framework. Be sure to check out the [repository][`compiletest`] and
the [rustc Dev-Guide][rustc-dev-guide] for more information about how it works,
how to configure it, and add new tests.

### Blessing Tests

You will occasionally need to "bless" the output from UI tests to update the
normalised output, you can do this by passing a `--bless` flag to
`cargo compiletest`.

```bash
cargo compiletest --bless
```

### Filtering Tests

When working on tests, you may need to run `cargo compiletest` a few times,
while changing only a small number of tests. You can avoid having to run all
the other (unrelated) tests, by passing substrings of their paths, to
`cargo compiletest`, for example:

```bash
cargo compiletest arch/u image
```

The above command will only test `ui/arch/u_*.rs` and `ui/image/*.rs`, and skip
everything else. You can also add `--bless` to update expected outputs, as well.

### Testing Different Environments

You can test against multiple different SPIR-V environments with the
`--target-env` flag. By default it is set to `unknown`.

```bash
cargo compiletest --target-env=vulkan1.1
# You can also provide multiple values to test multiple environments
cargo compiletest --target-env=vulkan1.1,spv.1.3
```

## Diff tests

### Adding Tests

To add a test, you need to write your GPU code twice--once in Rust, and once in WGSL.
Together these two shaders/kernels form a logical test. The testing harness will run
both programs, compare their output, and fail the test if they don't match. The location
of the test in the "pipelines/" directory determines the output type.

Tests can control the behavior of the test harness via comment directives in the
corresponding Rust code. Comment directives are not supported in WGSL.

### Fragment and Vertex shaders

Tests in "pipelines/graphics/" are used for vertex and fragment shaders. The test
harness will save the resulting display buffers to images and compare them, failing if
they do not match. If they do not match, their paths will be output so that you can
determine what is different.

You must currently name your fragment entrypoint `main_fs` and your vertex entry point
`main_vs`.

### Compute Kernels / Shaders

Tests in "pipelines/compute/" are used for compute shaders. The test harness will
compare the two binary outputs and fail the test if they don't match.

The output type is specified by a comment directive:

```
// output: [u8]
```

Valid values are `[u8]`, `f32`, and `u32`.

You must currently name your compute entrypoint `main_cs`.

### Fragment and Vertex with Compute Shaders

Tests in "pipelines/mixed/" are used for running vertex, fragment, and compute shaders
at the same time. As with vertex and fragment shaders, test harness will save the
resulting display buffers to images and compare them.

### Filtering Tests

Similar to compiletests, `cargo difftest` supports running specific tests by passing
substrings of their paths:

```bash
cargo difftest compute simple
```

[`compiletest`]: https://github.com/laumann/compiletest-rs
[rustc-dev-guide]: https://rustc-dev-guide.rust-lang.org/tests/intro.html
