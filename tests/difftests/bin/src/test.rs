use anyhow::Result;
use std::process::ExitCode;

pub fn main() -> Result<ExitCode> {
    difftest_runner::run()
}
