# Difftests

This folder contains tests known as "difftests". Each file in the `ui` folder corresponds to a
single difftest. The way they work is a tool iterates over every file, and tries to compile it.
At the start of the file, there's some meta-comments about the expected result of the compile:
whether it should succeed compilation, or fail. If it is expected to fail, there's a corresponding
.stderr file next to the file that contains the expected compiler error message.

The `src` folder here is the tool that iterates over every file in the `ui` folder.

You can run compiletests via `cargo difftest`. This is an alias set up in `.cargo/config` for
`cargo run --release -p difftest --`. You can filter to run specific tests by passing the
(partial) filenames to `cargo difftest some_file_name`.
