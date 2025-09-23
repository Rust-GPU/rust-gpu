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
   - If outputs differ, the test fails with detailed error reporting.
   - Tests can specify metadata to enable smarter epsilon-based comparisons and human
     display of data.

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
workspace rather than the main workspace in order to not pollute our root workspace and
slow down cargo due to evaluating the potentially hundreds of cargo projects in
difftests.

### Test Binary Example

Each test binary must:

1. Have a unique package name in `Cargo.toml`.
2. Read the config file path from `argv[1]`.
3. Load the config using `difftest::Config::from_path`.
4. Write its computed output to `output_path`.

The test binary can _optionally_ write test metadata to `metadata_path` for custom
comparison behavior.

For example:

```rust
use difftest::config::{Config, TestMetadata, OutputType};
use std::{env, fs, io::Write};

fn main() {
    let config_path = env::args().nth(1).expect("No config path provided");
    let config = Config::from_path(&config_path).expect("Invalid config");

    // Real work would go here.
    let output = compute_test_output();

    let mut file = fs::File::create(&config.output_path)
        .expect("Failed to create output file");
    file.write_all(&output).expect("Failed to write output");

    // Optional: Write metadata for floating-point comparison
    config.write_metadata(&TestMetadata::f32(0.00001)).expect("Failed to write metadata");
}
```

### Common test types

Of course, many test will have common host and GPU needs. Rather than require every test
binary to reimplement functionality, we have created some common tests with reasonable
defaults in the `difftest` library.

The library provides helper types for common test patterns:

**Test types:**

- `WgpuComputeTest` - Single buffer compute shader test
- `WgpuComputeTestMultiBuffer` - Multi-buffer compute shader test with input/output
  separation
- `WgpuComputeTestPushConstants` - Compute shader test with push constants support
- `Skip` - Marks a test variant as skipped with a reason

**Shader source types:**

- `RustComputeShader` - Compiles the current crate as a Rust GPU shader
- `WgslComputeShader` - Loads WGSL shader from file (shader.wgsl or compute.wgsl)

**Backend types:**

- `WgpuBackend` - Default wgpu-based compute backend
- `AshBackend` - Ash-based compute backend (low-level Vulkan access for debugging driver issues)

For examples, see:

- [`tests/lang/core/ops/math_ops/`](tests/lang/core/ops/math_ops/) - Multi-buffer test
  with floating-point metadata
- [`tests/arch/push_constants/`](tests/arch/push_constants/) - Push
  constants usage
- [`tests/arch/workgroup_memory/`](tests/arch/workgroup_memory/) - Workgroup memory
  usage

### Test Metadata

Tests producing floating-point outputs can specify comparison metadata to handle
platform-specific precision differences. The metadata controls how the harness compares
outputs:

```rust
use difftest::config::{TestMetadata, OutputType};

// Write metadata before or after writing output
let metadata = TestMetadata::f32(0.00001); // output is f32 with some epsilon
config.write_metadata(&metadata)?;

// Alternative: Construct TestMetadata yourself 
let metadata = TestMetadata {
    epsilon: Some(0.00001),        // Maximum allowed epsilon / difference (default: None)
    output_type: OutputType::F32,  // How to interpret output data (default: Raw)
    ..TestMetadata::default()
};
config.write_metadata(&metadata)?;
```

**Metadata fields:**

- `epsilon`: Optional maximum allowed absolute difference between values. When `None`
  (default), exact byte-for-byte comparison is used. When `Some(value)`, floating-point
  values are compared with the specified tolerance.
- `output_type`: Specifies how to interpret output data:
  - `Raw`: Exact byte comparison (default)
  - `F32`: Interpret as array of 32-bit floats, enables epsilon comparison
  - `F64`: Interpret as array of 64-bit floats, enables epsilon comparison
  - `U32`/`I32`: Interpret as 32-bit integers (epsilon ignored)

**Important notes:**

- If no metadata file is written or the file is empty, the harness uses exact byte
  comparison.
- All test packages must have consistent metadata. If packages specify different
  `output_type` values, the test will fail with an error.
- Invalid JSON in metadata files will cause the test to fail immediately.
- The `epsilon` field is only used when `output_type` is `F32` or `F64`.

## Running Tests

### Install cargo-nextest:

```sh
cargo install cargo-nextest
```

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

When outputs differ, the harness provides detailed error reporting:

### For raw byte differences

- Shows which packages produced different outputs
- Lists output file paths for manual inspection
- Groups packages by their output values

Inspect the output files with your preferred tools to determine the root cause.

### For floating-point differences (with `output_type: F32/F64`)

Reports all of the above, plus:

- Actual floating-point values in a comparison table
- Shows the maximum difference found
- Indicates the epsilon threshold (if specified)
- Highlights specific values that exceed the tolerance

### Additional output files

The harness automatically writes human-readable `.txt` files alongside binary outputs.
For floating-point data (F32/F64), these show the array values in decimal format. For
raw/integer data, these show the values as hex bytes or integers

## Skipping Tests on Specific Platforms

Sometimes a test variant needs to be skipped on certain platforms (e.g., due to driver 
issues or platform limitations). The difftest framework provides a clean way to handle
this using the `Skip` scaffolding type:

```rust
use difftest::scaffold::Skip;

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    
    // Skip on macOS due to platform-specific issues
    #[cfg(target_os = "macos")]
    {
        let skip = Skip::new("This test is not supported on macOS");
        skip.run_test(&config).unwrap();
        return;
    }
    
    // Run the actual test on other platforms
    #[cfg(not(target_os = "macos"))]
    {
        // ... normal test implementation ...
    }
}
```

When a test is skipped:
- The skip reason is recorded in the test metadata
- The test runner logs the skip reason
- The test doesn't contribute to the output comparison
- If all variants are skipped, the test fails with an error

## Harness logs

If you suspect a bug in the test harness, you can view detailed test harness logs:

```sh
RUST_LOG=trace cargo difftest
```
