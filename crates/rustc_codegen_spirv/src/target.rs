use rspirv::spirv::MemoryModel;
use rustc_codegen_spirv_types::{SPIRV_ARCH, SPIRV_TARGET_PREFIX, SPIRV_VENDOR};
use rustc_target::spec::{Cc, LinkerFlavor, PanicStrategy, Target, TargetOptions};
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::str::FromStr;
use strum::{Display, EnumString, IntoStaticStr};

#[derive(Clone, Eq, PartialEq)]
pub enum TargetError {
    /// If during parsing a target variant returns `UnknownTarget`, further variants will attempt to parse the string.
    /// Returning another error means that you have recognized the target but something else is invalid, and we should
    /// abort the parsing with your error.
    UnknownTarget(String),
    InvalidTargetVersion(SpirvTarget),
    InvalidNagaVariant(String),
}

impl Display for TargetError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TargetError::UnknownTarget(target) => {
                write!(f, "Unknown target `{target}`")
            }
            TargetError::InvalidTargetVersion(target) => {
                write!(f, "Invalid version in target `{}`", target.env())
            }
            TargetError::InvalidNagaVariant(target) => {
                write!(f, "Unknown naga out variant `{target}`")
            }
        }
    }
}

impl Debug for TargetError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

/// A version with a major and minor component
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Version(pub u8, pub u8);

impl Version {
    pub const fn new(major: u8, minor: u8) -> Self {
        Self(major, minor)
    }

    pub const fn from_tuple(tuple: (u8, u8)) -> Self {
        Self(tuple.0, tuple.1)
    }

    pub const fn to_tuple(self) -> (u8, u8) {
        (self.0, self.1)
    }

    fn parse_unbounded(s: &str) -> Option<(Self, &str)> {
        fn parse_num(s: &str) -> Option<(&str, u8)> {
            let mut value = 0;
            let mut len = 0;
            for digit in s.as_bytes().iter().copied() {
                if !digit.is_ascii_digit() {
                    break;
                }
                if value == 0 && len > 0 {
                    return None;
                }
                value = value * 10 + (digit - b'0') as u64;
                len += 1;
            }
            (len > 0).then_some((&s[len..], u8::try_from(value).ok()?))
        }
        let (s, major) = parse_num(s)?;
        if !matches!(s.chars().next(), Some('.')) {
            return None;
        }
        let s = &s[1..];
        let (s, minor) = parse_num(s)?;
        Some((Self(major, minor), s))
    }
}

impl FromStr for Version {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (out, s) = Self::parse_unbounded(s).ok_or(())?;
        s.is_empty().then_some(out).ok_or(())
    }
}

#[inline]
fn from_str_to_parse_unbounded<T: SpirvTargetVariant>(
    s: &str,
    parse_unbounded: impl FnOnce(&str) -> Option<(T, &str)>,
) -> Result<T, TargetError> {
    let unknown = || TargetError::UnknownTarget(s.to_owned());
    let (out, s) = parse_unbounded(s).ok_or_else(unknown)?;
    if !s.is_empty() {
        return Err(unknown());
    }
    out.validate()?;
    Ok(out)
}

impl Display for Version {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.0, self.1)
    }
}

impl PartialOrd<Self> for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0).then(self.1.cmp(&other.1))
    }
}

/// A SPIR-V version
///
/// For the SPIR-V universal target, see [`UniversalTarget`]
#[repr(transparent)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct SpirvVersion {
    pub version: Version,
}

impl SpirvVersion {
    pub const V1_0: Self = Self::new(1, 0);
    pub const V1_1: Self = Self::new(1, 1);
    pub const V1_2: Self = Self::new(1, 2);
    pub const V1_3: Self = Self::new(1, 3);
    pub const V1_4: Self = Self::new(1, 4);
    pub const V1_5: Self = Self::new(1, 5);
    pub const V1_6: Self = Self::new(1, 6);

    #[inline]
    pub const fn new(major: u8, minor: u8) -> Self {
        Self {
            version: Version::new(major, minor),
        }
    }
}

impl From<Version> for SpirvVersion {
    fn from(version: Version) -> Self {
        Self { version }
    }
}

impl Deref for SpirvVersion {
    type Target = Version;

    fn deref(&self) -> &Self::Target {
        &self.version
    }
}

impl DerefMut for SpirvVersion {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.version
    }
}

/// A trait to describe common properties between different target variants
pub trait SpirvTargetVariant {
    /// Validate the target version
    fn validate(&self) -> Result<(), TargetError>;
    /// Get the [`spirv_tools::TargetEnv`] to use for `spirv-val` and `spirv-opt`. May panic if version is invalid.
    fn to_spirv_tools(&self) -> spirv_tools::TargetEnv;
    /// Get the [`SpirvVersion`] of this target. May panic if version is invalid.
    fn spirv_version(&self) -> SpirvVersion;
}

/// A SPIR-V universal target
///
/// This is different from [`SpirvVersion`] to prevent misuse! [`Self::spirv_version`] doesn't return a
/// [`UniversalTarget`] but a [`SpirvVersion`], which can't accidentally be passed onwards as a target. So you can't
/// accidentally turn a [`VulkanTarget`] target into a [`UniversalTarget`].
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct UniversalTarget {
    pub version: Version,
}

impl UniversalTarget {
    pub const UNIVERSAL_1_0: Self = Self::new(Version(1, 0));
    pub const UNIVERSAL_1_1: Self = Self::new(Version(1, 1));
    pub const UNIVERSAL_1_2: Self = Self::new(Version(1, 2));
    pub const UNIVERSAL_1_3: Self = Self::new(Version(1, 3));
    pub const UNIVERSAL_1_4: Self = Self::new(Version(1, 4));
    pub const UNIVERSAL_1_5: Self = Self::new(Version(1, 5));
    pub const UNIVERSAL_1_6: Self = Self::new(Version(1, 6));
    pub const ALL_UNIVERSAL_TARGETS: &'static [Self] = &[
        Self::UNIVERSAL_1_0,
        Self::UNIVERSAL_1_1,
        Self::UNIVERSAL_1_2,
        Self::UNIVERSAL_1_3,
        Self::UNIVERSAL_1_4,
        Self::UNIVERSAL_1_5,
        Self::UNIVERSAL_1_6,
    ];

    pub const fn new(version: Version) -> Self {
        Self { version }
    }

    pub const fn properties(self) -> Result<spirv_tools::TargetEnv, TargetError> {
        Ok(match self.version {
            Version(1, 0) => spirv_tools::TargetEnv::Universal_1_0,
            Version(1, 1) => spirv_tools::TargetEnv::Universal_1_1,
            Version(1, 2) => spirv_tools::TargetEnv::Universal_1_2,
            Version(1, 3) => spirv_tools::TargetEnv::Universal_1_3,
            Version(1, 4) => spirv_tools::TargetEnv::Universal_1_4,
            Version(1, 5) => spirv_tools::TargetEnv::Universal_1_5,
            Version(1, 6) => spirv_tools::TargetEnv::Universal_1_6,
            _ => {
                return Err(TargetError::InvalidTargetVersion(SpirvTarget::Universal(
                    self,
                )));
            }
        })
    }
}

impl SpirvTargetVariant for UniversalTarget {
    fn validate(&self) -> Result<(), TargetError> {
        self.properties()?;
        Ok(())
    }

    fn to_spirv_tools(&self) -> spirv_tools::TargetEnv {
        self.properties().unwrap()
    }

    fn spirv_version(&self) -> SpirvVersion {
        SpirvVersion::from(self.version)
    }
}

impl UniversalTarget {
    fn parse_unbounded(s: &str) -> Option<(Self, &str)> {
        let s = s.strip_prefix("spv")?;
        let (version, s) = Version::parse_unbounded(s)?;
        Some((Self::new(version), s))
    }
}

impl FromStr for UniversalTarget {
    type Err = TargetError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        from_str_to_parse_unbounded(s, Self::parse_unbounded)
    }
}

impl Display for UniversalTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "spv{}", self.version)
    }
}

/// A Vulkan target
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VulkanTarget {
    pub version: Version,
    /// optional, may specify a spv version
    pub spv_version: Option<Version>,
}

impl VulkanTarget {
    pub const VULKAN_1_0: Self = Self::new(Version(1, 0));
    pub const VULKAN_1_1: Self = Self::new(Version(1, 1));
    pub const VULKAN_1_1_SPV_1_4: Self = Self {
        version: Version(1, 1),
        spv_version: Some(Version(1, 4)),
    };
    pub const VULKAN_1_2: Self = Self::new(Version(1, 2));
    pub const VULKAN_1_3: Self = Self::new(Version(1, 3));
    pub const VULKAN_1_4: Self = Self::new(Version(1, 4));
    pub const ALL_VULKAN_TARGETS: &'static [Self] = &[
        Self::VULKAN_1_0,
        Self::VULKAN_1_1,
        Self::VULKAN_1_1_SPV_1_4,
        Self::VULKAN_1_2,
        Self::VULKAN_1_3,
        Self::VULKAN_1_4,
    ];

    pub const fn new(version: Version) -> Self {
        Self {
            version,
            spv_version: None,
        }
    }

    pub const fn properties(self) -> Result<(SpirvVersion, spirv_tools::TargetEnv), TargetError> {
        let err = Err(TargetError::InvalidTargetVersion(SpirvTarget::Vulkan(self)));
        Ok(match (self.version, self.spv_version) {
            (Version(1, 0), None) => (SpirvVersion::new(1, 0), spirv_tools::TargetEnv::Vulkan_1_0),
            (Version(1, 1), None) => (SpirvVersion::new(1, 3), spirv_tools::TargetEnv::Vulkan_1_1),
            (Version(1, 1), Some(Version(1, 4))) => (
                SpirvVersion::new(1, 4),
                spirv_tools::TargetEnv::Vulkan_1_1_Spirv_1_4,
            ),
            (Version(1, 2), None) => (SpirvVersion::new(1, 5), spirv_tools::TargetEnv::Vulkan_1_2),
            (Version(1, 3), None) => (SpirvVersion::new(1, 6), spirv_tools::TargetEnv::Vulkan_1_3),
            (Version(1, 4), None) => (SpirvVersion::new(1, 6), spirv_tools::TargetEnv::Vulkan_1_4),
            _ => return err,
        })
    }
}

impl SpirvTargetVariant for VulkanTarget {
    fn validate(&self) -> Result<(), TargetError> {
        self.properties()?;
        Ok(())
    }

    fn to_spirv_tools(&self) -> spirv_tools::TargetEnv {
        self.properties().unwrap().1
    }

    fn spirv_version(&self) -> SpirvVersion {
        self.properties().unwrap().0
    }
}

impl VulkanTarget {
    fn parse_unbounded(s: &str) -> Option<(Self, &str)> {
        let s = s.strip_prefix("vulkan")?;
        let (version, s) = Version::parse_unbounded(s)?;
        let (spv_version, s) = if !s.is_empty() {
            let s = s.strip_prefix("-spv")?;
            let (spv_version, s) = Version::parse_unbounded(s)?;
            (Some(spv_version), s)
        } else {
            (None, s)
        };
        Some((
            Self {
                version,
                spv_version,
            },
            s,
        ))
    }
}

impl FromStr for VulkanTarget {
    type Err = TargetError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        from_str_to_parse_unbounded(s, Self::parse_unbounded)
    }
}

impl Display for VulkanTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "vulkan{}", self.version)?;
        if let Some(spv_version) = self.spv_version {
            write!(f, "-spv{spv_version}")?;
        }
        Ok(())
    }
}

/// An OpenGL target
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct OpenGLTarget {
    pub version: Version,
}

impl OpenGLTarget {
    pub const OPENGL_4_0: Self = Self::new(Version(4, 0));
    pub const OPENGL_4_1: Self = Self::new(Version(4, 1));
    pub const OPENGL_4_2: Self = Self::new(Version(4, 2));
    pub const OPENGL_4_3: Self = Self::new(Version(4, 3));
    pub const OPENGL_4_5: Self = Self::new(Version(4, 5));
    pub const ALL_OPENGL_TARGETS: &'static [Self] = &[
        Self::OPENGL_4_0,
        Self::OPENGL_4_1,
        Self::OPENGL_4_2,
        Self::OPENGL_4_3,
        Self::OPENGL_4_5,
    ];

    pub const fn new(version: Version) -> Self {
        Self { version }
    }

    pub const fn properties(self) -> Result<spirv_tools::TargetEnv, TargetError> {
        Ok(match self.version {
            Version(4, 0) => spirv_tools::TargetEnv::OpenGL_4_0,
            Version(4, 1) => spirv_tools::TargetEnv::OpenGL_4_1,
            Version(4, 2) => spirv_tools::TargetEnv::OpenGL_4_2,
            Version(4, 3) => spirv_tools::TargetEnv::OpenGL_4_3,
            Version(4, 5) => spirv_tools::TargetEnv::OpenGL_4_5,
            _ => {
                return Err(TargetError::InvalidTargetVersion(SpirvTarget::OpenGL(self)));
            }
        })
    }
}

impl SpirvTargetVariant for OpenGLTarget {
    fn validate(&self) -> Result<(), TargetError> {
        self.properties()?;
        Ok(())
    }

    fn to_spirv_tools(&self) -> spirv_tools::TargetEnv {
        self.properties().unwrap()
    }

    /// always 1.0
    fn spirv_version(&self) -> SpirvVersion {
        SpirvVersion::new(1, 0)
    }
}

impl OpenGLTarget {
    fn parse_unbounded(s: &str) -> Option<(Self, &str)> {
        let s = s.strip_prefix("opengl")?;
        let (version, s) = Version::parse_unbounded(s)?;
        Some((Self { version }, s))
    }
}

impl FromStr for OpenGLTarget {
    type Err = TargetError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        from_str_to_parse_unbounded(s, Self::parse_unbounded)
    }
}

impl Display for OpenGLTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "opengl{}", self.version)
    }
}

/// A naga target
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct NagaTarget {
    pub out: NagaOut,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, IntoStaticStr, Display, EnumString)]
#[allow(clippy::upper_case_acronyms)]
pub enum NagaOut {
    #[strum(to_string = "wgsl")]
    WGSL,
}

impl NagaTarget {
    pub const NAGA_WGSL: Self = NagaTarget::new(NagaOut::WGSL);
    pub const ALL_NAGA_TARGETS: &'static [Self] = &[Self::NAGA_WGSL];
    /// emit spirv like naga targets were this target
    pub const EMIT_SPIRV_LIKE: SpirvTarget = SpirvTarget::VULKAN_1_3;

    pub const fn new(out: NagaOut) -> Self {
        Self { out }
    }
}

impl SpirvTargetVariant for NagaTarget {
    fn validate(&self) -> Result<(), TargetError> {
        Ok(())
    }

    fn to_spirv_tools(&self) -> spirv_tools::TargetEnv {
        Self::EMIT_SPIRV_LIKE.to_spirv_tools()
    }

    fn spirv_version(&self) -> SpirvVersion {
        Self::EMIT_SPIRV_LIKE.spirv_version()
    }
}

impl FromStr for NagaTarget {
    type Err = TargetError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s
            .strip_prefix("naga-")
            .ok_or_else(|| TargetError::UnknownTarget(s.to_owned()))?;
        Ok(Self::new(FromStr::from_str(s).map_err(|_e| {
            TargetError::InvalidNagaVariant(s.to_owned())
        })?))
    }
}

impl Display for NagaTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "naga-{}", self.out)
    }
}

/// A rust-gpu target
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum SpirvTarget {
    Universal(UniversalTarget),
    Vulkan(VulkanTarget),
    OpenGL(OpenGLTarget),
    Naga(NagaTarget),
}

impl SpirvTarget {
    pub const UNIVERSAL_1_0: Self = Self::Universal(UniversalTarget::UNIVERSAL_1_0);
    pub const UNIVERSAL_1_1: Self = Self::Universal(UniversalTarget::UNIVERSAL_1_1);
    pub const UNIVERSAL_1_2: Self = Self::Universal(UniversalTarget::UNIVERSAL_1_2);
    pub const UNIVERSAL_1_3: Self = Self::Universal(UniversalTarget::UNIVERSAL_1_3);
    pub const UNIVERSAL_1_4: Self = Self::Universal(UniversalTarget::UNIVERSAL_1_4);
    pub const UNIVERSAL_1_5: Self = Self::Universal(UniversalTarget::UNIVERSAL_1_5);
    pub const UNIVERSAL_1_6: Self = Self::Universal(UniversalTarget::UNIVERSAL_1_6);
    pub const VULKAN_1_0: Self = Self::Vulkan(VulkanTarget::VULKAN_1_0);
    pub const VULKAN_1_1: Self = Self::Vulkan(VulkanTarget::VULKAN_1_1);
    pub const VULKAN_1_1_SPV_1_4: Self = Self::Vulkan(VulkanTarget::VULKAN_1_1_SPV_1_4);
    pub const VULKAN_1_2: Self = Self::Vulkan(VulkanTarget::VULKAN_1_2);
    pub const VULKAN_1_3: Self = Self::Vulkan(VulkanTarget::VULKAN_1_3);
    pub const VULKAN_1_4: Self = Self::Vulkan(VulkanTarget::VULKAN_1_4);
    pub const OPENGL_4_0: Self = Self::OpenGL(OpenGLTarget::OPENGL_4_0);
    pub const OPENGL_4_1: Self = Self::OpenGL(OpenGLTarget::OPENGL_4_1);
    pub const OPENGL_4_2: Self = Self::OpenGL(OpenGLTarget::OPENGL_4_2);
    pub const OPENGL_4_3: Self = Self::OpenGL(OpenGLTarget::OPENGL_4_3);
    pub const OPENGL_4_5: Self = Self::OpenGL(OpenGLTarget::OPENGL_4_5);
    pub const NAGA_WGSL: Self = Self::Naga(NagaTarget::NAGA_WGSL);

    #[allow(clippy::match_same_arms)]
    pub const fn memory_model(&self) -> MemoryModel {
        match self {
            SpirvTarget::Universal(_) => MemoryModel::Simple,
            SpirvTarget::Vulkan(_) => MemoryModel::Vulkan,
            SpirvTarget::OpenGL(_) => MemoryModel::GLSL450,
            SpirvTarget::Naga(_) => MemoryModel::Vulkan,
        }
    }
}

impl SpirvTargetVariant for SpirvTarget {
    fn validate(&self) -> Result<(), TargetError> {
        match self {
            SpirvTarget::Universal(t) => t.validate(),
            SpirvTarget::Vulkan(t) => t.validate(),
            SpirvTarget::OpenGL(t) => t.validate(),
            SpirvTarget::Naga(t) => t.validate(),
        }
    }

    fn to_spirv_tools(&self) -> spirv_tools::TargetEnv {
        match self {
            SpirvTarget::Universal(t) => t.to_spirv_tools(),
            SpirvTarget::Vulkan(t) => t.to_spirv_tools(),
            SpirvTarget::OpenGL(t) => t.to_spirv_tools(),
            SpirvTarget::Naga(t) => t.to_spirv_tools(),
        }
    }

    fn spirv_version(&self) -> SpirvVersion {
        match self {
            SpirvTarget::Universal(t) => t.spirv_version(),
            SpirvTarget::Vulkan(t) => t.spirv_version(),
            SpirvTarget::OpenGL(t) => t.spirv_version(),
            SpirvTarget::Naga(t) => t.spirv_version(),
        }
    }
}

impl SpirvTarget {
    pub fn parse_env(s: &str) -> Result<Self, TargetError> {
        let mut result;
        result = UniversalTarget::from_str(s).map(Self::Universal);
        if matches!(result, Err(TargetError::UnknownTarget(..))) {
            result = VulkanTarget::from_str(s).map(Self::Vulkan);
        }
        if matches!(result, Err(TargetError::UnknownTarget(..))) {
            result = OpenGLTarget::from_str(s).map(Self::OpenGL);
        }
        if matches!(result, Err(TargetError::UnknownTarget(..))) {
            result = NagaTarget::from_str(s).map(Self::Naga);
        }
        result
    }

    pub fn parse_target(s: &str) -> Result<Self, TargetError> {
        let s = s
            .strip_prefix(SPIRV_TARGET_PREFIX)
            .ok_or_else(|| TargetError::UnknownTarget(s.to_owned()))?;
        Self::parse_env(s)
    }

    pub fn parse(s: &str) -> Result<Self, TargetError> {
        Self::parse_env(s.strip_prefix(SPIRV_TARGET_PREFIX).unwrap_or(s))
    }

    /// returns the target env, e.g. `vulkan1.3`
    pub fn env(&self) -> String {
        match self {
            SpirvTarget::Universal(t) => t.to_string(),
            SpirvTarget::Vulkan(t) => t.to_string(),
            SpirvTarget::OpenGL(t) => t.to_string(),
            SpirvTarget::Naga(t) => t.to_string(),
        }
    }

    /// returns the full target, e.g. `spirv-unknown-vulkan1.3`
    pub fn target(&self) -> String {
        format!("{}{}", SPIRV_TARGET_PREFIX, self.env())
    }

    pub fn all_targets() -> impl Iterator<Item = Self> {
        UniversalTarget::ALL_UNIVERSAL_TARGETS
            .iter()
            .map(|t| Self::Universal(*t))
            .chain(
                VulkanTarget::ALL_VULKAN_TARGETS
                    .iter()
                    .map(|t| Self::Vulkan(*t)),
            )
            .chain(
                OpenGLTarget::ALL_OPENGL_TARGETS
                    .iter()
                    .map(|t| Self::OpenGL(*t)),
            )
            .chain(NagaTarget::ALL_NAGA_TARGETS.iter().map(|t| Self::Naga(*t)))
    }
}

impl SpirvTarget {
    pub fn rustc_target(&self) -> Target {
        let mut o = TargetOptions::default();
        o.simd_types_indirect = false;
        o.allows_weak_linkage = false;
        o.crt_static_allows_dylibs = true;
        o.crt_static_respected = true;
        o.dll_prefix = "".into();
        o.dll_suffix = ".spv.json".into();
        o.dynamic_linking = true;
        o.emit_debug_gdb_scripts = false;
        o.linker_flavor = LinkerFlavor::Unix(Cc::No);
        o.panic_strategy = PanicStrategy::Abort;
        o.env = self.env().into();
        o.vendor = SPIRV_VENDOR.into();
        // TODO: Investigate if main_needs_argc_argv is useful (for building exes)
        o.main_needs_argc_argv = false;

        Target {
            llvm_target: self.target().into(),
            metadata: Default::default(),
            pointer_width: 32,
            data_layout: "e-m:e-p:32:32:32-i64:64-n8:16:32:64".into(),
            arch: SPIRV_ARCH.into(),
            options: o,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prefix_matches_arch_vendor() {
        assert_eq!(SPIRV_TARGET_PREFIX, format!("{SPIRV_ARCH}-{SPIRV_VENDOR}-"));
    }

    #[test]
    fn test_str_roundtrip_target() {
        for target in SpirvTarget::all_targets() {
            assert_eq!(SpirvTarget::parse_target(&target.target()), Ok(target));
        }
    }

    #[test]
    fn test_str_roundtrip_env() {
        for target in SpirvTarget::all_targets() {
            assert_eq!(SpirvTarget::parse_env(&target.env()), Ok(target));
        }
    }

    #[test]
    fn test_unknown_target() {
        let result = SpirvTarget::parse_env("unknown");
        assert!(
            matches!(result, Err(TargetError::UnknownTarget(..))),
            "{result:?}",
        );
        let result = SpirvTarget::parse_env("vulkan6.8");
        assert!(
            matches!(result, Err(TargetError::InvalidTargetVersion(..))),
            "{result:?}",
        );
        let result = SpirvTarget::parse_env("vulkan1.4-spv1.0");
        assert!(
            matches!(result, Err(TargetError::InvalidTargetVersion(..))),
            "{result:?}",
        );
        let result = SpirvTarget::parse_env("spv1.4-spv1.0");
        assert!(
            matches!(result, Err(TargetError::UnknownTarget(..))),
            "{result:?}",
        );
    }
}
