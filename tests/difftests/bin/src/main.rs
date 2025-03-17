use anyhow::Result;
use std::{env, process};
use tester::{
    ColorConfig, DynTestName, OutputFormat, RunIgnored, ShouldPanic, TestDesc, TestDescAndFn,
    TestFn, TestType, run_tests_console,
    test::{TestOpts, parse_opts},
};
use tracing_subscriber::FmtSubscriber;

mod runner;
use runner::Runner;

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
            eprintln!("Error parsing test options: {}", e);
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

    let runner = Runner::new(base.clone());

    let test_cases =
        Runner::collect_test_dirs(&base).expect("Failed to collect test case directories");
    if test_cases.is_empty() {
        eprintln!("No valid tests found in {}", base.display());
        process::exit(1);
    }

    let tests: Vec<TestDescAndFn> = test_cases
        .into_iter()
        .map(|case| {
            let test_name = Runner::format_test_name(&case, &base);
            TestDescAndFn {
                desc: TestDesc {
                    name: DynTestName(test_name.clone()),
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
            }
        })
        .collect();

    let passed = run_tests_console(&opts, tests).expect("Failed to run tests");

    process::exit(if passed { 0 } else { 1 });
}
