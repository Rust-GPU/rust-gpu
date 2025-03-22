use bytesize::ByteSize;
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    fs,
    io::Write,
    path::{Component, Path, PathBuf},
    process::Command,
};
use tempfile::NamedTempFile;
use thiserror::Error;
use tracing::{debug, error, info, trace};

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

#[derive(Clone)]
pub struct Runner {
    pub base_dir: PathBuf,
}

impl Runner {
    pub fn new(base_dir: PathBuf) -> Self {
        Self { base_dir }
    }

    pub fn run_test_case(&self, test_case: &Path) -> RunnerResult<()> {
        trace!("Starting test case: {}", test_case.display());
        let packages = self.collect_packages(test_case)?;
        debug!(
            "Found {} package(s) in test case {}",
            packages.len(),
            test_case.display()
        );
        if packages.len() < 2 {
            error!("Insufficient packages in test case {}", test_case.display());
            return Err(RunnerError::InsufficientPackages {
                count: packages.len(),
            });
        }

        // Pre-check that package names are globally unique.
        let mut names_seen = HashSet::new();
        for package in &packages {
            let manifest_path = package.join("Cargo.toml");
            let pkg_name = self.get_package_name(&manifest_path)?;
            if !names_seen.insert(pkg_name.clone()) {
                return Err(RunnerError::DuplicatePackageName { pkg_name });
            }
        }

        let mut temp_files: Vec<NamedTempFile> = Vec::with_capacity(packages.len());
        let mut pkg_outputs: Vec<PackageOutput> = Vec::with_capacity(packages.len());

        for package in packages {
            trace!("Processing package at {}", package.display());
            let manifest_path = package.join("Cargo.toml");
            let pkg_name = self.get_package_name(&manifest_path)?;
            debug!("Package '{}' detected", pkg_name);

            let output_file = NamedTempFile::new()?;
            let temp_output_path = output_file.path().to_path_buf();
            temp_files.push(output_file);
            trace!(
                "Temporary output file created at {}",
                temp_output_path.display()
            );

            let config = HarnessConfig {
                output_path: temp_output_path.clone(),
            };
            let config_json = serde_json::to_string(&config)
                .map_err(|e| RunnerError::Config { msg: e.to_string() })?;
            let mut config_file = NamedTempFile::new()?;
            write!(config_file, "{}", config_json).map_err(|e| RunnerError::Io { source: e })?;
            trace!("Config file created at {}", config_file.path().display());

            let mut cmd = Command::new("cargo");
            let cmd = cmd
                .arg("run")
                .arg("--release")
                .arg("--manifest-path")
                .arg(
                    manifest_path
                        .to_str()
                        .ok_or_else(|| RunnerError::Manifest {
                            path: manifest_path.clone(),
                        })?,
                )
                .arg(
                    config_file
                        .path()
                        .to_str()
                        .ok_or_else(|| RunnerError::Config {
                            msg: "Invalid config file path".into(),
                        })?,
                );
            debug!("Running cargo command: {:?}", cmd);

            let output = cmd
                .current_dir(&package)
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
                    package_path: package,
                    exit_status: exit_code,
                    stderr: stderr_str,
                });
            }

            let output_bytes = fs::read(temp_files.last().unwrap().path())?;
            debug!(
                "Read {} bytes of output for package '{}'",
                output_bytes.len(),
                pkg_name
            );
            pkg_outputs.push(PackageOutput {
                pkg_name,
                package_path: package,
                output: output_bytes,
                temp_path: temp_output_path,
            });
        }

        if pkg_outputs.iter().all(|po| po.output.is_empty()) {
            error!("All packages produced empty output.");
            return Err(RunnerError::EmptyOutput);
        }

        let groups = self.group_outputs(&pkg_outputs);
        if groups.len() > 1 {
            let details = self.format_group_details(&pkg_outputs);
            self.keep_temp_files(&mut temp_files);
            return Err(RunnerError::DifferingOutput(details));
        }
        info!(
            "Test case '{}' passed.",
            Runner::format_test_name(test_case, test_case.parent().unwrap_or(test_case))
        );
        Ok(())
    }

    #[allow(clippy::unused_self)]
    fn collect_packages(&self, test_case: &Path) -> RunnerResult<Vec<PathBuf>> {
        let mut packages = Vec::new();
        for entry in fs::read_dir(test_case)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() && path.join("Cargo.toml").exists() {
                debug!("Found package candidate: {}", path.display());
                packages.push(path);
            }
        }
        Ok(packages)
    }

    #[allow(clippy::unused_self)]
    fn group_outputs<'a>(
        &self,
        pkg_outputs: &'a [PackageOutput],
    ) -> HashMap<Vec<u8>, Vec<&'a PackageOutput>> {
        let mut groups: HashMap<Vec<u8>, Vec<&'a PackageOutput>> = HashMap::new();
        for po in pkg_outputs {
            groups.entry(po.output.clone()).or_default().push(po);
        }
        groups
    }

    fn format_group_details(&self, pkg_outputs: &[PackageOutput]) -> String {
        let groups = self.group_outputs(pkg_outputs);
        const TOTAL_WIDTH: usize = 50;
        let mut details = Vec::with_capacity(groups.len() * 4);
        for (i, (output, group)) in groups.iter().enumerate() {
            let group_index = i + 1;
            let header = format!(
                "╭─ Output {} ({}) ",
                group_index,
                ByteSize::b(output.len() as u64)
            );
            let header = if header.len() < TOTAL_WIDTH {
                format!("{}{}", header, "─".repeat(TOTAL_WIDTH - header.len()))
            } else {
                header
            };
            details.push(header);
            for po in group {
                let p = po
                    .package_path
                    .strip_prefix(self.base_dir.parent().expect("base_dir is not root"))
                    .expect("base_path is not a prefix of package_path");
                details.push(format!("│ {} ({})", po.pkg_name, p.display()));
            }
            let footer = format!("╰──▶ {} \n", group[0].temp_path.display());
            details.push(footer);
        }
        details.join("\n")
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

    #[allow(clippy::unused_self)]
    fn keep_temp_files(&self, temp_files: &mut Vec<NamedTempFile>) {
        for file in temp_files.drain(..) {
            let _ = file.into_temp_path().keep();
        }
    }

    pub fn format_test_name(test_case: &Path, base: &Path) -> String {
        let name = test_case.strip_prefix(base).map_or_else(
            |_| test_case.to_string_lossy().into_owned(),
            |relative| {
                relative
                    .components()
                    .filter_map(|comp| match comp {
                        Component::Normal(os_str) => Some(os_str.to_string_lossy()),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
                    .join("::")
            },
        );
        format!("difftests::{}", name)
    }

    pub fn collect_test_dirs(root: &Path) -> RunnerResult<Vec<PathBuf>> {
        let mut test_cases = Vec::new();
        for entry in fs::read_dir(root)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                if Runner::collect_test_binaries(&path)?.len() >= 2 {
                    debug!("Test case found: {}", path.display());
                    test_cases.push(path.clone());
                }
                let mut subdirs = Runner::collect_test_dirs(&path)?;
                test_cases.append(&mut subdirs);
            }
        }
        Ok(test_cases)
    }

    fn collect_test_binaries(test_case: &Path) -> RunnerResult<Vec<PathBuf>> {
        let mut packages = Vec::new();
        for entry in fs::read_dir(test_case)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() && path.join("Cargo.toml").exists() {
                debug!("Found binary package candidate: {}", path.display());
                packages.push(path);
            }
        }
        Ok(packages)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs, io::Write, path::Path, path::PathBuf};
    use tempfile::{NamedTempFile, tempdir};

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
        let runner = Runner::new(PathBuf::from("dummy_base"));
        let groups = runner.group_outputs(&outputs);
        assert_eq!(groups.len(), 2);
    }

    #[test]
    fn test_format_test_name() {
        let base = Path::new("/home/user/tests");
        let test_case = base.join("group1/testcase1");
        let formatted = Runner::format_test_name(&test_case, base);
        assert_eq!(formatted, "difftests::group1::testcase1");
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
        write!(temp, "{}", cargo_toml).expect("failed to write to temp file");
        let runner = Runner::new(PathBuf::from("dummy_base"));
        let pkg_name = runner
            .get_package_name(temp.path())
            .expect("failed to get package name");
        assert_eq!(pkg_name, "dummy_package");
    }

    #[test]
    fn test_collect_packages() {
        let temp_dir = tempdir().expect("failed to create temp dir");
        let dir_path = temp_dir.path();
        let pkg_dir = dir_path.join("pkg1");
        fs::create_dir(&pkg_dir).expect("failed to create pkg1 dir");
        fs::write(pkg_dir.join("Cargo.toml"), "[package]\nname = \"pkg1\"")
            .expect("failed to write Cargo.toml");
        let runner = Runner::new(PathBuf::from("dummy_base"));
        let packages = runner
            .collect_packages(dir_path)
            .expect("failed to collect packages");
        assert_eq!(packages.len(), 1);
        assert_eq!(packages[0], pkg_dir);
    }

    #[test]
    fn test_collect_test_dirs() {
        let temp_dir = tempdir().expect("failed to create temp dir");
        let base = temp_dir.path();
        let test_case_dir = base.join("test_case");
        fs::create_dir(&test_case_dir).expect("failed to create test_case dir");
        let pkg1_dir = test_case_dir.join("pkg1");
        fs::create_dir(&pkg1_dir).expect("failed to create pkg1");
        fs::write(pkg1_dir.join("Cargo.toml"), "[package]\nname = \"pkg1\"")
            .expect("failed to write Cargo.toml for pkg1");
        let pkg2_dir = test_case_dir.join("pkg2");
        fs::create_dir(&pkg2_dir).expect("failed to create pkg2");
        fs::write(pkg2_dir.join("Cargo.toml"), "[package]\nname = \"pkg2\"")
            .expect("failed to write Cargo.toml for pkg2");
        let test_dirs = Runner::collect_test_dirs(base).expect("failed to collect test dirs");
        assert!(test_dirs.contains(&test_case_dir));
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
        let runner = Runner::new(PathBuf::from("dummy_base"));
        let result = runner.run_test_case(&test_case_dir);
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
        let runner = Runner::new(PathBuf::from("dummy_base"));
        let result = runner.run_test_case(&test_case_dir);
        match result {
            Err(RunnerError::DuplicatePackageName { pkg_name }) => assert_eq!(pkg_name, "dup_pkg"),
            _ => panic!("Expected DuplicatePackageName error"),
        }
    }
}
