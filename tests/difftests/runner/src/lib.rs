#![allow(clippy::exit)]

use crate::testcase::collect_test_dirs;
use anyhow::Result;
use libtest_mimic::{Arguments, Trial};
use runner::Runner;
use std::process::ExitCode;
use std::sync::Arc;
use std::{
    env, fs,
    process::{self},
};
use tracing_subscriber::FmtSubscriber;

mod differ;
mod runner;
mod testcase;

pub fn run() -> Result<ExitCode> {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("Failed to set global subscriber");

    let mut args = Arguments::from_args();

    // If filters are provided that look like paths (contain '/'), convert them to test names
    if let Some(filter) = &mut args.filter {
        *filter = filter.replace('/', "::");
    }

    let tests = collect_tests()?;
    Ok(libtest_mimic::run(&args, tests).exit_code())
}

fn collect_tests() -> Result<Vec<Trial>> {
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

    let runner = Arc::new(Runner {
        base_dir: base.clone(),
        output_dir,
    });

    let test_cases = collect_test_dirs(&base).expect("Failed to collect test case directories");
    if test_cases.is_empty() {
        eprintln!("No valid tests found in {}", base.display());
        process::exit(1);
    }

    let trails = test_cases
        .into_iter()
        .map(|case| {
            let runner = runner.clone();
            Trial::test(case.to_string(), move || Ok(runner.run_test_case(&case)?))
        })
        .collect();
    Ok(trails)
}
