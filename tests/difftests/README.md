# Difftests

Difftests verify correctness by running multiple implementations of the same logic and
comparing their outputs. Instead of relying on fixed reference outputs, they detect
discrepancies across implementations.

## How It Works

1. **Test Discovery**

   - The harness scans `tests/` for test cases.
   - A test case is a directory containing at least two Rust binary packages / variants
     to compare.

2. **Configuration & Execution**

   - The harness creates a temporary output file for each variant binary.
   - The harness runs each test variant binary with `cargo run --release`.
   - A JSON config file with the output path and other settings is passed to the test
     binary as `argv[1]`.
   - The binary reads the config, runs its computation, and writes to the output file.
   - Tests are run serially so they have full control of the GPU.

3. **Output Comparison**
   - The harness reads outputs as opaque bytes.
   - If outputs differ, the test fails.

Because the difftest harness merely runs Rust binaries in a directory, it supports
testing various setups. For example, you can:

- Compare different CPU host code (`ash`, `wgpu`, etc.) with different GPU backends
  (`rust-gpu`, `cuda`, `metal`, `wgsl`, etc.) against each other to make sure the output
  is consistent.
- Verify that CPU and GPU implementations produce the same output.
- Ensure the same `rust-gpu` code gives identical results across different dispatch
  methods.

## Writing a Test

Create a subdirectory under `tests/` with the test name. For example, `tests/foo/` for a
test named `foo`. In the test directory, create 2 or more Rust binary packages. Add the
packages to the top-level workspace `Cargo.toml` in the `tests/` directory. _Note that
this isn't the top-level workspace for the project._ The test binaries are in their own
workspace rather than the main workspace.

### Test Binary Example

Each test binary must:

1. Have a unique package name in `Cargo.toml`.
2. Read the config file path from `argv[1]`.
3. Load the config using `difftest::Config::from_path`.
4. Write its computed output to `output_path`.

For example:

```rust
use difftest::config::Config;
use std::{env, fs, io::Write};

fn main() {
    let config_path = env::args().nth(1).expect("No config path provided");
    let config = Config::from_path(&config_path).expect("Invalid config");

    // Real work would go here.
    let output = compute_test_output();

    let mut file = fs::File::create(&config.output_path)
        .expect("Failed to create output file");
    file.write_all(&output).expect("Failed to write output");
}
```

### Common test types

Of course, many test will have common host and GPU needs. Rather than require every test
binary to reimplement functionality, we have created some common tests with reasonable
defaults in the `difftest` library.

For example, this will handle compiling the current crate as a Rust compute shader,
running it via `wgpu`, and writing the output to the appropriate place:

```rust
fn main() {
    // Load the config from the harness.
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // Define test parameters, loading the rust shader from the current crate.
    let test = WgpuComputeTest::new(RustComputeShader::default(), [1, 1, 1], 1024);

    // Run the test and write the output to a file.
    test.run_test(&config).unwrap();
}
```

and this will handle loading a shader named `shader.wgsl` or `compute.wgsl` in the root
of the current crate, running it via `wgpu`, and writing the output to the appropriate
place:

```rust
fn main() {
    // Load the config from the harness.
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

    // Define test parameters, loading the wgsl shader from the crate directory.
    let test = WgpuComputeTest::new(WgslComputeShader::default(), [1, 1, 1], 1024);

    // Run the test and write the output to a file.
    test.run_test(&config).unwrap();
}
```

## Running Tests

### Run all difftests:

```sh
cargo difftest
```

Note that `cargo difftest` is an alias in `.cargo/config` for `cargo run --release -p
difftest --`.

### Run specific tests by name:

```sh
cargo difftest some_test_name
```

### Show stdout/stderr from tests:

```sh
cargo difftest --nocapture
```

## Debugging Failing Tests

If outputs differ, the error message lists:

- Binary package names
- Their directories
- Output file paths

Inspect the output files with your preferred tools to determine the differences.

If you suspect a bug in the test harness, you can view detailed test harness logs:

```sh
RUST_LOG=trace cargo difftest
```
