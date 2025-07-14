#![allow(clippy::exit)]

use crate::testcase::collect_test_dirs;
use anyhow::Result;
use runner::Runner;
use std::{
    env, fs,
    process::{self, Command},
};
use tester::{
    ColorConfig, DynTestName, OutputFormat, RunIgnored, ShouldPanic, TestDesc, TestDescAndFn,
    TestFn, TestType, run_tests_console,
    test::{TestOpts, parse_opts},
};
use tracing_subscriber::FmtSubscriber;

mod differ;
mod runner;
mod testcase;

fn main() -> Result<()> {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("Failed to set global subscriber");

    let args: Vec<String> = env::args().collect();
    let opts: TestOpts = match parse_opts(&args) {
        Some(Ok(o)) => TestOpts {
            test_threads: Some(1),
            ..o
        },
        Some(Err(e)) => {
            eprintln!("Error parsing test options: {e}");
            process::exit(1);
        }
        None => TestOpts {
            list: false,
            filters: vec![],
            filter_exact: false,
            force_run_in_process: false,
            exclude_should_panic: false,
            run_ignored: RunIgnored::No,
            run_tests: true,
            bench_benchmarks: false,
            logfile: None,
            nocapture: false,
            color: ColorConfig::AutoColor,
            format: OutputFormat::Pretty,
            test_threads: Some(1),
            skip: vec![],
            time_options: None,
            options: tester::Options {
                display_output: true,
                panic_abort: true,
            },
        },
    };

    // Find the manifest directory at compile time and locate tests in ../tests.
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let base = std::path::Path::new(manifest_dir)
        .join("../tests")
        .canonicalize()
        .expect("Failed to canonicalize tests directory");
    tracing::debug!("Using tests directory: {}", base.display());

    let output_dir = std::path::Path::new(manifest_dir).join("../tests/target/difftest");
    fs::create_dir_all(&output_dir)?;
    let output_dir = output_dir
        .canonicalize()
        .expect("Failed to canonicalize tests directory");
    tracing::debug!("Using output directory: {}", output_dir.display());

    let runner = Runner {
        base_dir: base.clone(),
        output_dir,
    };

    let test_cases = collect_test_dirs(&base).expect("Failed to collect test case directories");
    if test_cases.is_empty() {
        eprintln!("No valid tests found in {}", base.display());
        process::exit(1);
    }

    // We build first to ensure that the tests are compiled before running them and to
    // passthrough stdout and stderr from cargo to help debugging.
    let mut cmd = Command::new("cargo");
    let cmd = cmd.arg("build").arg("--release");
    runner::forward_features(cmd);
    cmd.current_dir(&base)
        .stderr(process::Stdio::inherit())
        .stdout(process::Stdio::inherit());
    tracing::debug!("Running cargo command: {:?}", cmd);

    let output = cmd.output().expect("build output");
    let exit_code = output.status.code().unwrap_or(-1);
    tracing::debug!("Cargo build exited with code {}", exit_code);
    if !output.status.success() {
        tracing::error!("Cargo build failed");
        process::exit(exit_code);
    }

    let tests: Vec<TestDescAndFn> = test_cases
        .into_iter()
        .map(|case| TestDescAndFn {
            desc: TestDesc {
                name: DynTestName(case.to_string()),
                ignore: false,
                should_panic: ShouldPanic::No,
                allow_fail: false,
                test_type: TestType::IntegrationTest,
            },
            testfn: TestFn::DynTestFn(Box::new({
                let runner = runner.clone();
                move || {
                    runner
                        .run_test_case(&case)
                        .unwrap_or_else(|e| panic!("{}", e));
                }
            })),
        })
        .collect();

    // If filters are provided that look like paths (contain '/'), convert them to test names
    let opts = if opts.filters.iter().any(|f| f.contains('/')) {
        let mut new_opts = opts;
        new_opts.filters = new_opts
            .filters
            .into_iter()
            .map(|filter| {
                if filter.contains('/') {
                    // Convert path-like filter to test name format
                    filter.replace('/', "::")
                } else {
                    filter
                }
            })
            .collect();
        new_opts
    } else {
        opts
    };

    let passed = run_tests_console(&opts, tests).expect("Failed to run tests");

    process::exit(if passed { 0 } else { 1 });
}
