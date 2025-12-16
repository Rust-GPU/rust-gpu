use crate::SpirvTarget;
use semver::Version;
use std::ffi::OsString;
use std::path::Path;

/// Enum for different versions of target specs, to allow changing the target spec for different rust versions.
/// The version listed in the enum is always the minimum version to require said target spec, with the newest version
/// always at the top.
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug)]
pub enum TargetSpecVersion {
    /// Introduced in `489c3ee6fd63da3ca7cf2b15e1ee709d8e078aab` in the old v2 target spec way, later ported to here.
    /// remove `os: unknown`, add `crt-static-respected: true`
    Rustc_1_85_0,
    /// rustc 1.76 has been tested to correctly parse modern target spec jsons.
    /// Some later version requires them.
    /// Some earlier version fails with them (notably our 0.9.0 release).
    Rustc_1_76_0,
}

impl TargetSpecVersion {
    /// Format the `--target` arg. On newer rustc versions, will create a compatible target spec json file and return
    /// the absolute path to it, on older rustc versions may return the target name.
    pub fn target_arg(
        rustc_version: Version,
        target: &SpirvTarget,
        target_spec_folder: &Path,
    ) -> std::io::Result<OsString> {
        if let Some(target_spec) = Self::from_rustc_version(rustc_version) {
            std::fs::create_dir_all(target_spec_folder)?;
            let spec_file = target_spec_folder.join(format!("{}.json", target.target()));
            std::fs::write(&spec_file, target_spec.format_spec(target))?;
            Ok(std::fs::canonicalize(spec_file)?.into_os_string())
        } else {
            Ok(OsString::from(target.target()))
        }
    }

    /// Returns the version of the target spec required for a certain rustc version. May return `None` if the version
    /// is old enough to not need target specs.
    pub fn from_rustc_version(rustc_version: Version) -> Option<Self> {
        if rustc_version >= Version::new(1, 85, 0) {
            Some(Self::Rustc_1_85_0)
        } else if rustc_version >= Version::new(1, 76, 0) {
            Some(Self::Rustc_1_76_0)
        } else {
            None
        }
    }

    /// format the target spec json
    pub fn format_spec(&self, target: &SpirvTarget) -> String {
        let target_env = target.env();
        let extra = match self {
            TargetSpecVersion::Rustc_1_85_0 => r#""crt-static-respected": true,"#,
            TargetSpecVersion::Rustc_1_76_0 => r#""os": "unknown","#,
        };
        format!(
            r#"{{
  "allows-weak-linkage": false,
  "arch": "spirv",
  "crt-objects-fallback": "false",
  "crt-static-allows-dylibs": true,
  "data-layout": "e-m:e-p:32:32:32-i64:64-n8:16:32:64",
  "dll-prefix": "",
  "dll-suffix": ".spv.json",
  "dynamic-linking": true,
  "emit-debug-gdb-scripts": false,
  "env": "{target_env}",
  "linker-flavor": "unix",
  "linker-is-gnu": false,
  "llvm-target": "spirv-unknown-{target_env}",
  "main-needs-argc-argv": false,
  "metadata": {{
    "description": null,
    "host_tools": null,
    "std": null,
    "tier": null
  }},
  {extra}
  "panic-strategy": "abort",
  "simd-types-indirect": false,
  "target-pointer-width": "32"
}}"#
        )
    }
}
