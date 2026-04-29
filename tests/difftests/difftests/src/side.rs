use std::fmt::Formatter;

#[derive(Copy, Clone, Debug)]
pub struct SideConfig {
    pub float_epsilon: f32,
}

pub trait SideValue {
    fn check_equality(&self, other: &Self, config: &SideConfig) -> Result<(), >;

    fn report_error(&self, other: &Self, config: &SideConfig) -> anyhow::Result<String>;
}

impl SideValue for &str {
    fn check_equality(&self, other: &Self, config: &SideConfig) -> anyhow::Result<bool> {
        self == other
    }

    fn report_error(&self, other: &Self, config: &SideConfig) -> anyhow::Result<String> {
        let diff = dissimilar::diff(self, other);
        diff.iter()
            .map(|chunk| match chunk {
                dissimilar::Chunk::Equal(text) => Cow::Borrowed(text),
                dissimilar::Chunk::Delete(text) => {
                    Cow::Owned(format!("\x1b[4m\x1b[31m{}\x1b[0m", text))
                }
                dissimilar::Chunk::Insert(text) => {
                    Cow::Owned(format!("\x1b[4m\x1b[32m{}\x1b[0m", text))
                }
            })
            .collect()
    }
}

impl SideValue for u32 {
    fn check_equality(&self, other: &Self, _: &SideConfig) -> Result<(), String> {
        if self == other {
            Ok(())
        }
    }
}

pub struct DiffFormatter<'a> {
    pub f: Formatter<'a>,
    has_diff: bool,
}

impl DiffFormatter {
    pub fn 
}
