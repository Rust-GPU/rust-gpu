# "Codegen args" (flags/options) supported by the Rust-GPU codegen backend

Please keep in mind that many of these flags/options are for internal development, and may break output
unexpectedly and generally muck things up. Please only use these if you know what you're doing.

Help is also appreciated keeping this document up to date, "codegen args" flags/options may be
added/removed on an ad-hoc basis without much thought, as they're internal development tools, not a
public API - this documentation is only here because these flags/options may be helpful diagnosing
problems for others.

It's recommended that "codegen args" options that take paths to files or directories are set to full
paths, as the working directory of the compiler might be something wonky and unexpected, and it's
easier to set the full path.

---

## How to set "codegen args" flags/options

The most convenient method is relying on `spirv-builder` reading the `RUSTGPU_CODEGEN_ARGS` environment variable, e.g.:

```console
$ RUSTGPU_CODEGEN_ARGS="--no-spirv-val --dump-post-link=$PWD/postlink" cargo run -p example-runner-wgpu
...
    Finished release [optimized] target(s) in 15.15s

$ file postlink/*
postlink/module: Khronos SPIR-V binary, little-endian, version 0x010300, generator 0x1b0000
```

Notably, `RUSTGPU_CODEGEN_ARGS="--help"` can be used to see a "usage" message (which lists all the flags/options, including ones not listed in this document), via e.g. running a Cargo build that relies on `spirv-builder`.

However, it's only a convenient alias for for `RUSTGPU_RUSTFLAGS=-Cllvm-args="..."` (without having to expose the fact that LLVM's name is still attached to `rustc`'s interface for this functionality), and if in direct control of `rustc`, you can still pass such "codegen args" flags/options wrapped in `-C llvm-args="..."`.

---

## Historical note about past "environment variables"

Many of these flags/options were at one point, individual environment variable (e.g. the `--dump-pre-link` option used to be the environment variable `DUMP_PRE_LINK`).

However, that approach is prone to various failure modes, because the environment variables would not get registered as a "dependency" (without extra work that never happened), and the idea of "codegen args" fits better with existing practices (e.g. `rustc -C llvm-args="..."` for the LLVM codegen backend of `rustc`).

For more context see also [PR #959](https://github.com/EmbarkStudios/rust-gpu/pull/959), which made the transition to this system.

## Where are all the rest of the flags/options documented?

If you do run a build with `RUSTGPU_CODEGEN_ARGS="--help"` (or `-C llvm-args="--help"`), you will notice more flags/options than are listed in this documented.

This is a historical artifact: as mentioned above, these used to be environment variables, and this document only described those, without talking about the older "codegen args" at all.

While most of those flags are usually only exposed through higher-level `spirv-builder` APIs, it would be nice to have all of them documented in one place (eventually?).

---

## Debugging "codegen args" flags/options

As mentioned above, these form the bulk of "codegen args", but keep in mind the list is not exhaustive and you will want to check the full list with e.g. `RUSTGPU_CODEGEN_ARGS="--help"`.

### `--dump-mir DIR`

Dumps the MIR of every function rust-gpu encounters, to files in `DIR`. Yes, rustc does have options to do
this by default, but I always forget the syntax, and plumbing through the option to `spirv-builder`
is annoying, so this is handy to just hack an output.

_**FIXME(@eddyb)** this may be irrelevant now given `RUSTGPU_RUSTFLAGS`_

### `--dump-module-on-panic FILE`

If codegen panics, then write the (partially) emitted module to `FILE`. Note that this only exists
for codegen, if the linker panics, this option does nothing, sadly.

### `--dump-pre-link DIR`

Dumps all input modules to the linker, to files in `DIR`, before the linker touches them at all.

### `--dump-post-merge DIR`

Dumps the merged module, to a file in `DIR`, immediately after merging, but before the linker has done anything else
(including, well, linking the methods - `LinkageAttributes` will still exist, etc.). This is very
similar to `--dump-pre-link`, except it outputs only a single file, which might make grepping through
for stuff easier.

### `--dump-pre-inline DIR`

Dumps the module, to a file in `DIR`, immediately before the inliner pass runs.

### `--dump-post-inline DIR`

Dumps the module, to a file in `DIR`, immediately after the inliner pass runs.

### `--dump-post-split DIR`

Dumps the modules, to files in `DIR`, immediately after multimodule splitting.

### `--dump-post-link DIR`

Takes: path to directory

Dumps all output modules from the linker, to files in `DIR`. This may be multiple files due to the multimodule/module
splitting option, hence it takes a directory instead of a file path. This is the final output
binary before `spirv-opt` is executed, so it may be useful to output this to check if an issue is in
Rust-GPU, or in `spirv-opt`.

### `--specializer-dump-instances FILE`

Dumps to `FILE` all instances inferred by the specializer.

### `--no-spirv-val`

Disables running `spirv-val` on the final output. Spooky scary option, can cause invalid modules!

### `--no-spirv-opt`

Forcibly disables running `spirv-opt` on the final output, even if optimizations are enabled.

### `--no-compact-ids`

Disables compaction of SPIR-V IDs at the end of linking. Causes absolutely ginormous IDs to be
emitted. Useful if you're println debugging IDs in the linker (although spirv-opt will compact them
anyway, be careful).

### `--no-early-report-zombies`

Delays reporting zombies (aka "deferred errors") even further, to allow more legalization.
Currently this also replaces the zombie reporting with a SPIR-T-based version
(which may become the default in the future).

### `--no-infer-storage-classes`

Disables the old SPIR-V "Storage Class" (i.e. address space) inference pass,
to allow testing alternatives to it (such as SPIR-T `qptr` passes).

Note that this will produce illegal SPIR-V by default, and you need e.g.
`--spirt-passes=qptr` in order to regain legal "Storage Class" assignments  
(see [SPIR-T `qptr` PR](https://github.com/EmbarkStudios/spirt/pull/24) for more information on `qptr` in general)

### `--no-structurize`

Disables CFG structurization. Probably results in invalid modules.

### `--spirt` <sub>_(until 0.6.0)_</sub>

~~Note: as of `rust-gpu 0.6.0`, `SPIR-🇹` is enabled by default. Use `--no-spirt` to disable.~~
Note: as of `rust-gpu 0.8.0`, `SPIR-🇹` is always being used and cannot be disabled
(to reduce the cost of maintenance, testing and further feature development).

Enables using the experimental [`SPIR-🇹` shader IR framework](https://github.com/rust-gpu/spirt) in the linker - more specifically, this:
- adds a `SPIR-V -> SPIR-🇹 -> SPIR-V` roundtrip  
  (future `SPIR-🇹` passes would go in the middle of this, and eventually codegen might not produce `SPIR-V` at all)
- replaces the existing structurizer with `SPIR-🇹` structurization (which is more robust and can e.g. handle `OpPhi`s)
- runs some existing `SPIR-V` legalization/optimization passes (`mem2reg`) *before* inlining, instead of *only after* (as the `OpPhi`s they would produce are no longer an issue for structurization)

For more information, also see [the `SPIR-🇹` repository](https://github.com/rust-gpu/spirt).

### `--no-spirt` <sub>_(0.6.0 and 0.7.0)_</sup>

Note: as of `rust-gpu 0.8.0`, `SPIR-🇹` is always being used and cannot be disabled
(to reduce the cost of maintenance, testing and further feature development).

Note: if you were using `--no-spirt` to work around [`naga` issue #1977](https://github.com/gfx-rs/naga/issues/1977)  
(valid loops causing `The 'break' is used outside of a 'loop' or 'switch' context`),  
you may be able to `cargo update -p naga` to update to a fixed `naga` version  
(`0.11.1` for `wgpu 0.15`, `0.12.1` for `wgpu 0.16`, and any later versions).

Disables the [`SPIR-🇹` shader IR framework](https://github.com/rust-gpu/spirt) in the linker.

### `--spirt-passes PASSES`

Enable additional `SPIR-🇹` passes, as listed in `PASSES` (comma-separated).  
Their implementation can be found in [`rustc_codegen_spirv::linker::spirt_passes`](../../crates/rustc_codegen_spirv/src/linker/spirt_passes).

_Note: passes that are not already enabled by default are considered experimental and likely not ready for production use, this flag exists primarily for testing._

### `--dump-spirt-passes DIR`

Dump the `SPIR-🇹` module across passes (i.e. all of the versions before/after each pass), as a combined report, to a pair of files (`.spirt` and `.spirt.html`) in `DIR`.  
<sub>(the `.spirt.html` version of the report is the recommended form for viewing, as it uses tabling for versions, syntax-highlighting-like styling, and use->def linking)</sub>

### `--spirt-strip-custom-debuginfo-from-dumps`

When dumping (pretty-printed) `SPIR-🇹` (e.g. with `--dump-spirt-passes`), strip
all the custom (Rust-GPU-specific) debuginfo instructions, by converting them
to the standard SPIR-V debuginfo (which `SPIR-🇹` understands more directly).

The default (keeping the custom instructions) is more verbose, but also lossless,
if you want to see all instructions exactly as e.g. `--spirt-passes` see them.

### `--spirt-keep-debug-sources-in-dumps`

When dumping (pretty-printed) `SPIR-🇹` (e.g. with `--dump-spirt-passes`), preserve
all the "file contents debuginfo" (i.e. from SPIR-V `OpSource` instructions),
which will end up being included, in full, at the start of the dump.

The default (of hiding the file contents) is less verbose, but (arguably) lossier.

### `--spirt-keep-unstructured-cfg-in-dumps`

When dumping (pretty-printed) `SPIR-🇹` (e.g. with `--dump-spirt-passes`), include
the initial unstructured state, as well (i.e. just after lowering from SPIR-V).

The default (of only dumping structured SPIR-T) can have far less noisy dataflow,
but unstructured SPIR-T may be needed for e.g. debugging the structurizer itself.
