# Tracing

`rust-gpu` has a lot of [debug!](https://docs.rs/tracing/0.1/tracing/macro.debug.html)
(or trace!) calls, which print out logging information at many points. These are very
useful to at least narrow down the location of a bug if not to find it entirely, or just
to orient yourself as to why the compiler backend is doing a particular thing.

To see the logs, you need to set the `RUSTGPU_LOG` environment variable to your log filter (note the "GPU" in the name). The full syntax of the log filters can be found in the [rustdoc of tracing-subscriber](https://docs.rs/tracing-subscriber/0.2.24/tracing_subscriber/filter/struct.EnvFilter.html#directives).

Use `RUSTGPU_LOG_FORMAT` to control log output format (`"tree"`, `"flat"`, or `"json"`) and `RUSTGPU_LOG_COLOR` to manage color output (`"always"`, `"never"`, or `"auto"`).

To trace non-`rust-gpu` parts of the compiler, set the standard [`RUSTC_LOG`](https://rustc-dev-guide.rust-lang.org/tracing.html) environment variable.

## Replacements for old codegen arguments

Before `rust-gpu` supported tracing, there were special [codegen
arguments](./codegen-args.md) to aid observability. As of
[PR#196](https://github.com/Rust-GPU/rust-gpu/pull/196) the they
have been removed and replaced with the following:

- `--specializer-debug` &rarr; `RUSTGPU_LOG=rustc_codegen_spirv::specializer=debug`
- `--print-zombie` &rarr; `RUSTGPU_LOG=print_zombie=debug`
- `--print-all-zombie` &rarr; `RUSTGPU_LOG=print_all_zombie=debug`
