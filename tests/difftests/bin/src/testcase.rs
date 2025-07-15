use crate::runner::RunnerResult;
use std::fmt::{Display, Formatter};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::debug;

/// A test case containing multiple test binaries that should produce the same output
pub struct TestCase {
    /// The name of the testcase, as a rust mod path
    pub name: String,
    /// the relative path from the base `difftest` dir
    pub relative_path: PathBuf,
    /// the absolute path
    pub absolute_path: PathBuf,
    /// All the test binaries of this single test case.
    pub test_binaries: Vec<TestBinary>,
}

impl TestCase {
    pub fn new_empty(root: &Path, relative_path: &Path) -> Self {
        TestCase {
            name: format!(
                "difftests::{}",
                relative_path.to_string_lossy().replace("/", "::")
            ),
            absolute_path: root.join(relative_path),
            relative_path: relative_path.to_path_buf(),
            test_binaries: Vec::new(),
        }
    }

    pub fn try_new(root: &Path, relative_path: &Path) -> RunnerResult<Option<Self>> {
        let mut test_case = Self::new_empty(root, relative_path);
        test_case.collect_test_binaries()?;
        if !test_case.test_binaries.is_empty() {
            debug!("Test case found: {}", relative_path.display());
            Ok(Some(test_case))
        } else {
            Ok(None)
        }
    }

    fn collect_test_binaries(&mut self) -> RunnerResult<()> {
        for entry in fs::read_dir(&self.absolute_path)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() && path.join("Cargo.toml").exists() {
                debug!("Found binary package candidate: {}", path.display());
                self.test_binaries
                    .push(TestBinary::new(self, PathBuf::from(entry.file_name())));
            }
        }
        Ok(())
    }
}

impl Display for TestCase {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

/// A test binary that can be executed
pub struct TestBinary {
    /// The name of the testcase, as a rust mod path
    pub name: String,
    /// the relative path from the base `difftest` dir
    pub relative_path: PathBuf,
    /// the absolute path
    pub absolute_path: PathBuf,
}

impl TestBinary {
    pub fn new(test_case: &TestCase, relative_to_test_case: PathBuf) -> Self {
        Self {
            name: format!(
                "{}::{}",
                test_case.name,
                relative_to_test_case.to_string_lossy().replace("/", "::")
            ),
            relative_path: test_case.relative_path.join(&relative_to_test_case),
            absolute_path: test_case.absolute_path.join(&relative_to_test_case),
        }
    }
}

impl Display for TestBinary {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

pub fn collect_test_dirs(root: &Path) -> RunnerResult<Vec<TestCase>> {
    fn recurse(root: &Path, traverse: &Path, test_cases: &mut Vec<TestCase>) -> RunnerResult<()> {
        let absolute_path = root.join(traverse);
        // skip target dir
        if absolute_path.file_name() == Some(std::ffi::OsStr::new("target")) {
            return Ok(());
        }

        if let Some(test_case) = TestCase::try_new(root, traverse)? {
            test_cases.push(test_case);
        }
        for entry in fs::read_dir(absolute_path)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                let relative_path = traverse.join(entry.file_name());
                recurse(root, &relative_path, test_cases)?;
            }
        }
        Ok(())
    }

    let mut test_cases = Vec::new();
    recurse(root, Path::new(""), &mut test_cases)?;
    Ok(test_cases)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_format_test_name() {
        let mut test_case =
            TestCase::new_empty(Path::new("/home/user/tests"), Path::new("core/group1"));
        test_case
            .test_binaries
            .push(TestBinary::new(&test_case, PathBuf::from("testcase1")));
        assert_eq!(test_case.to_string(), "difftests::core::group1");
        assert_eq!(
            test_case.test_binaries[0].to_string(),
            "difftests::core::group1::testcase1"
        );
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
        let mut test_dirs = collect_test_dirs(base).expect("failed to collect test dirs");

        assert_eq!(test_dirs.len(), 1);
        let test_case = &mut test_dirs[0];
        assert_eq!(test_case.relative_path.to_string_lossy(), "test_case");
        assert_eq!(test_case.absolute_path, test_case_dir);

        test_case
            .test_binaries
            .sort_by(|a, b| a.relative_path.cmp(&b.relative_path));
        assert_eq!(
            test_case.test_binaries[0].relative_path.to_string_lossy(),
            "test_case/pkg1"
        );
        assert_eq!(test_case.test_binaries[0].absolute_path, pkg1_dir);
        assert_eq!(
            test_case.test_binaries[1].relative_path.to_string_lossy(),
            "test_case/pkg2"
        );
        assert_eq!(test_case.test_binaries[1].absolute_path, pkg2_dir);
    }
}
