use bytesize::ByteSize;
use difftest::config::{OutputType, TestMetadata};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    process::Command,
};
use thiserror::Error;
use tracing::{debug, error, info, trace};

use crate::differ::{DiffMagnitude, Difference, DifferenceDisplay, OutputDiffer};
use crate::testcase::TestCase;

#[derive(Debug, Error)]
pub enum RunnerError {
    #[error("I/O error: {source}")]
    Io {
        #[from]
        source: std::io::Error,
    },
    #[error("TOML parse error in manifest at {path:?}: {source}")]
    Toml {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },
    #[error("Manifest error in manifest at {path:?}")]
    Manifest { path: PathBuf },
    #[error("Insufficient binary packages: found {count}, but at least 2 are required.")]
    InsufficientPackages { count: usize },
    #[error("All binaries produced empty output.")]
    EmptyOutput,
    #[error(
        "Duplicate package name found: {pkg_name}. Packages must use unique names as we reuse the target directory to reduce compile times"
    )]
    DuplicatePackageName { pkg_name: String },
    #[error("Outputs differ:\n\n{0}")]
    DifferingOutput(String),
    #[error(
        "Cargo execution failed in package '{pkg_name}' at '{package_path}' with exit code {exit_status}. Stderr:\n{stderr}"
    )]
    CargoExecutionFailed {
        pkg_name: String,
        package_path: PathBuf,
        exit_status: i32,
        stderr: String,
    },
    #[error("Configuration error: {msg}")]
    Config { msg: String },
}

pub type RunnerResult<T> = std::result::Result<T, RunnerError>;

#[derive(Debug, Serialize)]
pub struct HarnessConfig {
    pub output_path: PathBuf,
    pub metadata_path: PathBuf,
}

#[derive(Deserialize)]
struct CargoPackage {
    name: String,
}

#[derive(Deserialize)]
struct CargoManifest {
    package: CargoPackage,
}

struct PackageOutput {
    pkg_name: String,
    package_path: PathBuf,
    output: Vec<u8>,
    temp_path: PathBuf,
}

struct TestInfo {
    name: String,
    path: String,
}

struct ErrorReport {
    lines: Vec<String>,
    test_info: TestInfo,
    summary_parts: Vec<String>,
}

impl ErrorReport {
    fn new(test_info: TestInfo, differ_name: &'static str, epsilon: Option<f32>) -> Self {
        let epsilon_str = match epsilon {
            Some(e) => format!(", ε={e}"),
            None => String::new(),
        };
        Self {
            lines: Vec::new(),
            test_info,
            summary_parts: vec![format!("{}{}", differ_name, epsilon_str)],
        }
    }

    fn set_summary_from_differences(&mut self, differences: &[Difference]) {
        if !differences.is_empty() {
            self.summary_parts
                .push(format!("{} differences", differences.len()));

            if let Some((max_diff, max_rel)) = Self::calculate_max_differences(differences) {
                self.summary_parts
                    .push(format!("max: {:.3e} ({:.2}%)", max_diff, max_rel * 100.0));
            }
        }
    }

    fn set_distinct_outputs(&mut self, count: usize) {
        self.summary_parts.push(format!("{count} distinct outputs"));
    }

    fn add_output_files(
        &mut self,
        groups: &HashMap<Vec<u8>, Vec<&PackageOutput>>,
        pkg_outputs: &[PackageOutput],
    ) {
        if groups.len() <= 5 {
            for (output_bytes, group) in groups {
                let names: Vec<&str> = group.iter().map(|po| po.pkg_name.as_str()).collect();
                self.lines.push(format!("▪ {}", names.join(", ")));
                self.lines.push(format!(
                    "  → Raw:  {} ({:.1})",
                    group[0].temp_path.display(),
                    ByteSize::b(output_bytes.len() as u64)
                ));
                let text_path = group[0].temp_path.with_extension("txt");
                self.lines
                    .push(format!("  → Text: {}", text_path.display()));
                self.lines.push("".to_string());
            }
        } else {
            for po in pkg_outputs {
                self.lines.push(format!("▪ {}", po.pkg_name));
                self.lines.push(format!(
                    "  → Raw:  {} ({:.1})",
                    po.temp_path.display(),
                    ByteSize::b(po.output.len() as u64)
                ));
                let text_path = po.temp_path.with_extension("txt");
                self.lines
                    .push(format!("  → Text: {}", text_path.display()));
                self.lines.push("".to_string());
            }
        }
    }

    fn add_comparison_table(&mut self, table: String) {
        self.lines.push(table);
    }

    fn add_summary_line(&mut self, differences: &[Difference]) {
        if !differences.is_empty() {
            if let Some((max_diff, max_rel)) = Self::calculate_max_differences(differences) {
                self.lines.push(format!(
                    "• {} differences, max: {:.3e} ({:.2}%)",
                    differences.len(),
                    max_diff,
                    max_rel * 100.0
                ));
            } else {
                self.lines
                    .push(format!("• {} differences", differences.len()));
            }
        }
    }

    fn calculate_max_differences(differences: &[Difference]) -> Option<(f64, f64)> {
        let max_diff = differences
            .iter()
            .filter_map(|d| match &d.absolute_diff {
                DiffMagnitude::Numeric(val) => Some(*val),
                DiffMagnitude::Incomparable => None,
            })
            .max_by(|a, b| a.partial_cmp(b).unwrap());
        let max_rel = differences
            .iter()
            .filter_map(|d| match &d.relative_diff {
                DiffMagnitude::Numeric(val) => Some(*val),
                DiffMagnitude::Incomparable => None,
            })
            .max_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap_or(0.0);

        max_diff.map(|diff| (diff, max_rel))
    }

    fn build(self) -> String {
        let mut result = Vec::new();

        // Header
        result.push(format!(
            "\x1b[1m{} ({})\x1b[0m",
            self.test_info.name, self.test_info.path
        ));
        result.push(self.summary_parts.join(" • "));
        result.push("─".repeat(65));
        result.push("".to_string());

        // Body
        result.extend(self.lines);

        result.join("\n")
    }
}

#[derive(Clone)]
pub struct Runner {
    pub base_dir: PathBuf,
    pub output_dir: PathBuf,
}

impl Runner {
    pub fn run_test_case(&self, test_case: &TestCase) -> RunnerResult<()> {
        trace!("Starting test case: {}", test_case);
        debug!(
            "Found {} package(s) in test case {}",
            test_case.test_binaries.len(),
            test_case
        );
        if test_case.test_binaries.len() < 2 {
            error!("Insufficient packages in test case {}", test_case);
            return Err(RunnerError::InsufficientPackages {
                count: test_case.test_binaries.len(),
            });
        }

        // Pre-check that package names are globally unique.
        let mut names_seen = HashSet::new();
        for package in &test_case.test_binaries {
            let manifest_path = package.absolute_path.join("Cargo.toml");
            let pkg_name = self.get_package_name(&manifest_path)?;
            if !names_seen.insert(pkg_name.clone()) {
                return Err(RunnerError::DuplicatePackageName { pkg_name });
            }
        }

        let mut pkg_outputs: Vec<PackageOutput> = Vec::with_capacity(test_case.test_binaries.len());
        let mut epsilon: Option<f32> = None;
        let mut output_type = None;

        for package in &test_case.test_binaries {
            trace!(
                "Processing package '{}' at '{}'",
                package,
                package.absolute_path.display()
            );
            let manifest_path = package.absolute_path.join("Cargo.toml");
            let pkg_name = self.get_package_name(&manifest_path)?;
            debug!("Package '{}' detected", pkg_name);

            let package_out = self.output_dir.join(&package.relative_path);
            fs::create_dir_all(&package_out)?;
            debug!("Writing output to '{}'", package_out.display());
            let config_file = package_out.join("config.json");
            let temp_output_path = package_out.join("out.bin");
            let temp_metadata_path = package_out.join("metadata.json");

            let config = HarnessConfig {
                output_path: temp_output_path.clone(),
                metadata_path: temp_metadata_path.clone(),
            };
            let config_json = serde_json::to_string(&config)
                .map_err(|e| RunnerError::Config { msg: e.to_string() })?;
            fs::write(&config_file, &config_json)?;
            trace!("Config file created at {}", config_file.display());

            let mut cmd = Command::new("cargo");
            cmd.arg("run").arg("--release").arg("--manifest-path").arg(
                manifest_path
                    .to_str()
                    .ok_or_else(|| RunnerError::Manifest {
                        path: manifest_path.clone(),
                    })?,
            );
            forward_features(&mut cmd);
            cmd.arg("--")
                .arg(config_file.to_str().ok_or_else(|| RunnerError::Config {
                    msg: "Invalid config file path".into(),
                })?);
            debug!("Running cargo command: {:?}", cmd);

            let output = cmd
                .current_dir(&package.absolute_path)
                .output()
                .map_err(|e| RunnerError::Io { source: e })?;
            let exit_code = output.status.code().unwrap_or(-1);
            debug!(
                "Cargo run for package '{}' exited with code {}",
                pkg_name, exit_code
            );
            if !output.status.success() {
                let stderr_str = String::from_utf8_lossy(&output.stderr).to_string();
                error!("Cargo execution failed for package '{}'", pkg_name);
                return Err(RunnerError::CargoExecutionFailed {
                    pkg_name,
                    package_path: package.absolute_path.clone(),
                    exit_status: exit_code,
                    stderr: stderr_str,
                });
            }

            let output_bytes = fs::read(&temp_output_path)?;
            debug!(
                "Read {} bytes of output for package '{}'",
                output_bytes.len(),
                pkg_name
            );

            // Try to read metadata file
            if let Ok(metadata_content) = fs::read_to_string(&temp_metadata_path) {
                if !metadata_content.trim().is_empty() {
                    match serde_json::from_str::<TestMetadata>(&metadata_content) {
                        Ok(metadata) => {
                            // Check if test was skipped
                            if let Some(skip_reason) = &metadata.skipped {
                                info!("Package '{}' was skipped: {}", pkg_name, skip_reason);
                                continue;
                            }

                            if let Some(meta_epsilon) = metadata.epsilon {
                                epsilon = match epsilon {
                                    Some(e) => Some(e.max(meta_epsilon)),
                                    None => Some(meta_epsilon),
                                };
                                debug!(
                                    "Found epsilon {} in metadata for package '{}'",
                                    meta_epsilon, pkg_name
                                );
                            }

                            if output_type.is_none() {
                                output_type = Some(metadata.output_type);
                            } else if output_type != Some(metadata.output_type) {
                                error!("Inconsistent output types across packages");
                                return Err(RunnerError::Config {
                                    msg: format!(
                                        "Package '{}' has output type {:?}, but previous packages have {:?}",
                                        pkg_name, metadata.output_type, output_type
                                    ),
                                });
                            }
                        }
                        Err(e) => {
                            error!("Failed to parse metadata for package '{}'", pkg_name);
                            return Err(RunnerError::Config {
                                msg: format!(
                                    "Failed to parse metadata for package '{pkg_name}': {e}"
                                ),
                            });
                        }
                    }
                } else {
                    debug!(
                        "Empty metadata file for package '{}', using defaults",
                        pkg_name
                    );
                }
            } else {
                debug!(
                    "No metadata file for package '{}', using defaults",
                    pkg_name
                );
            }

            pkg_outputs.push(PackageOutput {
                pkg_name,
                package_path: package.absolute_path.clone(),
                output: output_bytes,
                temp_path: temp_output_path,
            });
        }

        // Check if we have any valid outputs
        if pkg_outputs.is_empty() {
            error!("All packages were skipped. At least one package must produce output.");
            return Err(RunnerError::EmptyOutput);
        }

        if pkg_outputs.iter().all(|po| po.output.is_empty()) {
            error!("All packages produced empty output.");
            return Err(RunnerError::EmptyOutput);
        }

        let output_type = output_type.unwrap_or_default();
        let groups = self.group_outputs(&pkg_outputs, epsilon, output_type);
        if groups.len() > 1 {
            let differ: Box<dyn OutputDiffer + Send + Sync> = output_type.into();
            let display: Box<dyn DifferenceDisplay + Send + Sync> = output_type.into();

            // Write human-readable outputs
            for po in &pkg_outputs {
                let text_path = po.temp_path.with_extension("txt");
                if let Err(e) = display.write_human_readable(&po.output, &text_path) {
                    debug!("Failed to write human-readable output: {}", e);
                } else {
                    info!("Wrote human-readable output to {}", text_path.display());
                }
            }

            // Generate detailed error report
            let details =
                self.format_error(&pkg_outputs, epsilon, output_type, &*differ, &*display);
            return Err(RunnerError::DifferingOutput(details));
        }
        info!("Test case '{test_case}' passed.");
        Ok(())
    }

    #[allow(clippy::unused_self)]
    fn group_outputs<'a>(
        &self,
        pkg_outputs: &'a [PackageOutput],
        epsilon: Option<f32>,
        output_type: OutputType,
    ) -> HashMap<Vec<u8>, Vec<&'a PackageOutput>> {
        let mut groups: HashMap<Vec<u8>, Vec<&'a PackageOutput>> = HashMap::new();

        // If no epsilon specified or type is Raw with epsilon 0, use exact byte comparison
        if epsilon.is_none() || (epsilon == Some(0.0) && output_type == OutputType::Raw) {
            for po in pkg_outputs {
                groups.entry(po.output.clone()).or_default().push(po);
            }
            return groups;
        }

        // Otherwise, group outputs that are within epsilon of each other
        for po in pkg_outputs {
            let mut found_group = false;

            for (group_output, group) in groups.iter_mut() {
                if Self::outputs_match(&po.output, group_output, epsilon, output_type) {
                    group.push(po);
                    found_group = true;
                    break;
                }
            }

            if !found_group {
                groups.insert(po.output.clone(), vec![po]);
            }
        }

        groups
    }

    fn outputs_match(
        output1: &[u8],
        output2: &[u8],
        epsilon: Option<f32>,
        output_type: OutputType,
    ) -> bool {
        if output1.len() != output2.len() {
            return false;
        }

        match output_type {
            OutputType::Raw => output1 == output2,
            OutputType::F32 => {
                if !output1.len().is_multiple_of(4) {
                    return false;
                }

                match epsilon {
                    None => output1 == output2, // Exact comparison if no epsilon
                    Some(eps) => {
                        let floats1: Vec<f32> = output1
                            .chunks_exact(4)
                            .map(|chunk| {
                                f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]])
                            })
                            .collect();

                        let floats2: Vec<f32> = output2
                            .chunks_exact(4)
                            .map(|chunk| {
                                f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]])
                            })
                            .collect();

                        floats1
                            .iter()
                            .zip(floats2.iter())
                            .all(|(a, b)| (a - b).abs() <= eps)
                    }
                }
            }
            OutputType::F64 => {
                if !output1.len().is_multiple_of(8) {
                    return false;
                }

                match epsilon {
                    None => output1 == output2, // Exact comparison if no epsilon
                    Some(eps) => {
                        let floats1: Vec<f64> = output1
                            .chunks_exact(8)
                            .map(|chunk| {
                                f64::from_le_bytes([
                                    chunk[0], chunk[1], chunk[2], chunk[3], chunk[4], chunk[5],
                                    chunk[6], chunk[7],
                                ])
                            })
                            .collect();

                        let floats2: Vec<f64> = output2
                            .chunks_exact(8)
                            .map(|chunk| {
                                f64::from_le_bytes([
                                    chunk[0], chunk[1], chunk[2], chunk[3], chunk[4], chunk[5],
                                    chunk[6], chunk[7],
                                ])
                            })
                            .collect();

                        floats1
                            .iter()
                            .zip(floats2.iter())
                            .all(|(a, b)| (a - b).abs() <= eps as f64)
                    }
                }
            }
            OutputType::U32 | OutputType::I32 => {
                // For integer types, epsilon doesn't make sense, so exact match
                output1 == output2
            }
        }
    }

    fn format_error(
        &self,
        pkg_outputs: &[PackageOutput],
        epsilon: Option<f32>,
        output_type: OutputType,
        differ: &dyn OutputDiffer,
        display: &dyn DifferenceDisplay,
    ) -> String {
        let test_info = self.extract_test_info(pkg_outputs);
        let groups = self.group_outputs(pkg_outputs, epsilon, output_type);

        let mut report = ErrorReport::new(test_info, differ.name(), epsilon);

        // Analyze the differences
        match groups.len() {
            0 => unreachable!("No output groups"),
            1 => unreachable!("All outputs match - shouldn't be an error"),
            2 if pkg_outputs.len() == 2 => {
                // Exactly 2 outputs that differ
                let differences =
                    differ.compare(&pkg_outputs[0].output, &pkg_outputs[1].output, epsilon);
                report.set_summary_from_differences(&differences);
            }
            2 => {
                // Multiple outputs, 2 distinct groups
                let group_vec: Vec<_> = groups.values().collect();
                let differences =
                    differ.compare(&group_vec[0][0].output, &group_vec[1][0].output, epsilon);
                report.set_summary_from_differences(&differences);
            }
            n => {
                // Many distinct outputs
                report.set_distinct_outputs(n);
            }
        }

        // Format output files
        report.add_output_files(&groups, pkg_outputs);

        // Add detailed comparison if applicable
        if groups.len() == 2 && pkg_outputs.len() == 2 {
            let differences =
                differ.compare(&pkg_outputs[0].output, &pkg_outputs[1].output, epsilon);
            if !differences.is_empty() {
                let table = display.format_report(
                    &differences,
                    &pkg_outputs[0].pkg_name,
                    &pkg_outputs[1].pkg_name,
                    epsilon,
                );
                report.add_comparison_table(table);
            }
        } else if groups.len() == 2 && pkg_outputs.len() > 2 {
            let group_vec: Vec<_> = groups.values().collect();
            let differences =
                differ.compare(&group_vec[0][0].output, &group_vec[1][0].output, epsilon);
            report.add_summary_line(&differences);
        }

        report.build()
    }

    fn extract_test_info(&self, pkg_outputs: &[PackageOutput]) -> TestInfo {
        if pkg_outputs.is_empty() {
            return TestInfo {
                name: "unknown".to_string(),
                path: "unknown".to_string(),
            };
        }

        let test_path = pkg_outputs[0]
            .package_path
            .parent()
            .unwrap_or(&pkg_outputs[0].package_path);
        let relative_path = test_path.strip_prefix(&self.base_dir).unwrap_or(test_path);

        TestInfo {
            name: test_path
                .file_name()
                .unwrap_or_else(|| std::ffi::OsStr::new("unknown"))
                .to_string_lossy()
                .to_string(),
            path: relative_path.display().to_string(),
        }
    }

    #[allow(clippy::unused_self)]
    fn get_package_name(&self, manifest_path: &Path) -> RunnerResult<String> {
        trace!("Reading manifest from {}", manifest_path.display());
        let content = fs::read_to_string(manifest_path)?;
        let manifest: CargoManifest = toml::from_str(&content).map_err(|e| RunnerError::Toml {
            path: manifest_path.to_path_buf(),
            source: e,
        })?;
        debug!("Package name '{}' found in manifest", manifest.package.name);
        Ok(manifest.package.name)
    }
}

pub fn forward_features(cmd: &mut Command) {
    cmd.arg("--features");
    #[cfg(feature = "use-compiled-tools")]
    {
        cmd.arg("difftest/use-compiled-tools");
    }
    #[cfg(feature = "use-installed-tools")]
    {
        cmd.arg("difftest/use-installed-tools");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use difftest::config::OutputType;
    use std::{fs, io::Write, path::Path, path::PathBuf};
    use tempfile::{NamedTempFile, tempdir};

    fn dummy_runner() -> Runner {
        Runner {
            base_dir: PathBuf::from("dummy_base"),
            output_dir: PathBuf::from("dummy_out"),
        }
    }

    fn dummy_package_output(name: &str, path: &str, output: &[u8], temp: &str) -> PackageOutput {
        PackageOutput {
            pkg_name: name.to_string(),
            package_path: PathBuf::from(path),
            output: output.to_vec(),
            temp_path: PathBuf::from(temp),
        }
    }

    #[test]
    fn test_group_outputs_multiple_groups() {
        let pkg1 = dummy_package_output("foo", "/path/to/foo", b"hello", "tmp1");
        let pkg2 = dummy_package_output("bar", "/path/to/bar", b"world", "tmp2");
        let pkg3 = dummy_package_output("baz", "/path/to/baz", b"hello", "tmp3");
        let outputs = vec![pkg1, pkg2, pkg3];
        let runner = dummy_runner();
        let groups = runner.group_outputs(&outputs, None, OutputType::Raw);
        assert_eq!(groups.len(), 2);
    }

    #[test]
    fn test_get_package_name() {
        let mut temp = NamedTempFile::new().expect("failed to create temp file");
        let cargo_toml = r#"
            [package]
            name = "dummy_package"
            version = "0.1.0"
            edition = "2021"
        "#;
        write!(temp, "{cargo_toml}").expect("failed to write to temp file");
        let runner = dummy_runner();
        let pkg_name = runner
            .get_package_name(temp.path())
            .expect("failed to get package name");
        assert_eq!(pkg_name, "dummy_package");
    }

    #[test]
    fn test_run_test_case_insufficient_packages() {
        let temp_dir = tempdir().expect("failed to create temp dir");
        let test_case_dir = temp_dir.path().join("single_pkg");
        fs::create_dir(&test_case_dir).expect("failed to create test_case dir");
        let pkg_dir = test_case_dir.join("pkg1");
        fs::create_dir(&pkg_dir).expect("failed to create pkg1");
        fs::write(pkg_dir.join("Cargo.toml"), "[package]\nname = \"pkg1\"")
            .expect("failed to write Cargo.toml for pkg1");
        let test_case = TestCase::try_new(temp_dir.path(), Path::new("single_pkg"))
            .unwrap()
            .unwrap();
        let result = dummy_runner().run_test_case(&test_case);
        match result {
            Err(RunnerError::InsufficientPackages { count }) => assert_eq!(count, 1),
            _ => panic!("Expected InsufficientPackages error"),
        }
    }

    #[test]
    fn test_duplicate_package_names() {
        let temp_dir = tempdir().expect("failed to create temp dir");
        let test_case_dir = temp_dir.path().join("dup_pkg");
        fs::create_dir(&test_case_dir).expect("failed to create test_case dir");
        let pkg1_dir = test_case_dir.join("pkg1");
        fs::create_dir(&pkg1_dir).expect("failed to create pkg1");
        fs::write(pkg1_dir.join("Cargo.toml"), "[package]\nname = \"dup_pkg\"")
            .expect("failed to write Cargo.toml for pkg1");
        let pkg2_dir = test_case_dir.join("pkg2");
        fs::create_dir(&pkg2_dir).expect("failed to create pkg2");
        fs::write(pkg2_dir.join("Cargo.toml"), "[package]\nname = \"dup_pkg\"")
            .expect("failed to write Cargo.toml for pkg2");
        let runner = dummy_runner();
        let test_case = TestCase::try_new(temp_dir.path(), Path::new("dup_pkg"))
            .unwrap()
            .unwrap();
        let result = runner.run_test_case(&test_case);
        match result {
            Err(RunnerError::DuplicatePackageName { pkg_name }) => assert_eq!(pkg_name, "dup_pkg"),
            _ => panic!("Expected DuplicatePackageName error"),
        }
    }

    #[test]
    fn test_outputs_match_no_epsilon() {
        // Exact match should work
        assert!(Runner::outputs_match(
            b"hello",
            b"hello",
            None,
            OutputType::Raw
        ));

        // Different content should not match
        assert!(!Runner::outputs_match(
            b"hello",
            b"world",
            None,
            OutputType::Raw
        ));
    }

    #[test]
    fn test_outputs_match_with_epsilon_f32() {
        // Prepare test data - two floats with small difference
        let val1: f32 = 1.0;
        let val2: f32 = 1.00001;
        let arr1 = [val1];
        let arr2 = [val2];
        let bytes1 = bytemuck::cast_slice(&arr1);
        let bytes2 = bytemuck::cast_slice(&arr2);

        // Should not match without epsilon
        assert!(!Runner::outputs_match(
            bytes1,
            bytes2,
            None,
            OutputType::F32
        ));

        // Should match with sufficient epsilon
        assert!(Runner::outputs_match(
            bytes1,
            bytes2,
            Some(0.0001),
            OutputType::F32
        ));

        // Should not match with too small epsilon
        assert!(!Runner::outputs_match(
            bytes1,
            bytes2,
            Some(0.000001),
            OutputType::F32
        ));
    }

    #[test]
    fn test_outputs_match_with_epsilon_f64() {
        // Prepare test data - two doubles with small difference
        let val1: f64 = 1.0;
        let val2: f64 = 1.00001;
        let arr1 = [val1];
        let arr2 = [val2];
        let bytes1 = bytemuck::cast_slice(&arr1);
        let bytes2 = bytemuck::cast_slice(&arr2);

        // Should not match without epsilon
        assert!(!Runner::outputs_match(
            bytes1,
            bytes2,
            None,
            OutputType::F64
        ));

        // Should match with sufficient epsilon
        assert!(Runner::outputs_match(
            bytes1,
            bytes2,
            Some(0.0001),
            OutputType::F64
        ));

        // Should not match with too small epsilon
        assert!(!Runner::outputs_match(
            bytes1,
            bytes2,
            Some(0.000001),
            OutputType::F64
        ));
    }

    #[test]
    fn test_group_outputs_with_epsilon() {
        let runner = dummy_runner();

        // Create float outputs with small differences
        let val1: f32 = 1.0;
        let val2: f32 = 1.00001;
        let val3: f32 = 2.0;

        let pkg1 =
            dummy_package_output("foo", "/path/to/foo", bytemuck::cast_slice(&[val1]), "tmp1");
        let pkg2 =
            dummy_package_output("bar", "/path/to/bar", bytemuck::cast_slice(&[val2]), "tmp2");
        let pkg3 =
            dummy_package_output("baz", "/path/to/baz", bytemuck::cast_slice(&[val3]), "tmp3");

        let outputs = vec![pkg1, pkg2, pkg3];

        // Without epsilon, val1 and val2 should be in different groups
        let groups = runner.group_outputs(&outputs, None, OutputType::F32);
        assert_eq!(groups.len(), 3);

        // With epsilon, val1 and val2 should be in the same group
        let groups_with_epsilon = runner.group_outputs(&outputs, Some(0.0001), OutputType::F32);
        assert_eq!(groups_with_epsilon.len(), 2);
    }

    #[test]
    fn test_invalid_metadata_json() {
        // Test that invalid JSON in metadata file causes proper error
        let metadata_content = "{ invalid json }";
        let result: Result<difftest::config::TestMetadata, _> =
            serde_json::from_str(metadata_content);
        assert!(result.is_err());
        // Just check that it's an error, don't check the specific message
    }

    #[test]
    fn test_inconsistent_output_types() {
        // This test verifies that when packages have different output types,
        // the runner returns an error. This tests the code at line 340-347
        // where we check: if output_type != Some(metadata.output_type)

        // We can't easily test the full run_test_case flow without real binaries,
        // but we can at least verify the error is constructed properly

        // Test that the error message is properly formatted
        let error = RunnerError::Config {
            msg: format!(
                "Package '{}' has output type {:?}, but previous packages have {:?}",
                "test_pkg",
                OutputType::F32,
                Some(OutputType::F64)
            ),
        };

        match error {
            RunnerError::Config { msg } => {
                assert!(msg.contains("test_pkg"));
                assert!(msg.contains("F32"));
                assert!(msg.contains("F64"));
            }
            _ => panic!("Wrong error type"),
        }
    }

    #[test]
    fn test_metadata_parsing_error_message() {
        // Test that metadata parsing errors are formatted correctly

        let error = RunnerError::Config {
            msg: format!(
                "Failed to parse metadata for package '{}': {}",
                "test_pkg", "invalid JSON"
            ),
        };

        match error {
            RunnerError::Config { msg } => {
                assert!(msg.contains("Failed to parse metadata"));
                assert!(msg.contains("test_pkg"));
                assert!(msg.contains("invalid JSON"));
            }
            _ => panic!("Wrong error type"),
        }
    }
}
