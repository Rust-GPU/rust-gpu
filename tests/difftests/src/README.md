# Difftests

This folder contains "difftests." The test harness compiles and runs Rust
shaders/kernels and compares the output with those produced by equivalent WGSL
shaders/kernels. Unlike traditional reference, golden, or snapshot tests—which check
against static outputs—differential testing compares two independent implementations.
This approach is more robust because discrepancies between them are more likely to
indicate real bugs rather than issues with outdated reference files.

You can run difftests via `cargo difftest`. This is an alias set up in `.cargo/config`
for `cargo run --release -p difftest --`. You can filter to run specific tests by
passing the (partial) filenames to `cargo difftest some_file_name`.
