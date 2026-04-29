use std::fmt::{Formatter, Write};

type Result = std::fmt::Result;

pub trait Diff {
    fn fmt(f: &mut Formatter<'_>, pad: Padding, a: Self, b: Self) -> Result;
}

#[derive(Copy, Clone, Default)]
pub struct Padding {
    pad: u32,
}

impl Padding {
    pub fn new(pad: u32) -> Self {
        Self { pad }
    }

    pub fn pad(&self, fmt: &mut Formatter<'_>) -> Result {
        for _ in 0..self.pad {
            fmt.write_char(' ')?;
        }
        Ok(())
    }

    pub fn add(&self, pad: u32) -> Padding {
        Padding {
            pad: self.pad + pad,
        }
    }
}

pub const PADDING_TAB: u32 = 4;

pub struct StructDiff<'a, 'b: 'a> {
    fmt: &'b mut Formatter<'a>,
    pad: Padding,
    result: Result,
    has_fields: bool,
}

impl<'a, 'b: 'a> StructDiff<'a, 'b> {
    pub fn new(fmt: &'b mut Formatter<'a>, pad: Padding, name: &str) -> Self {
        Self {
            result: write!(fmt, "{name} {{"),
            fmt,
            pad,
            has_fields: false,
        }
    }

    pub fn field<T: Diff>(&mut self, name: &str, value_a: &T, value_b: &T) -> &mut self {
        self.result = self.result.and_then(|_| {
            self.pad.add(PADDING_TAB).pad(&mut self.fmt)?;
            write!(self.fmt, "{name}: ")?;
            T::fmt(&mut self.fmt, self.pad, value_a, value_b)?;
            write!(self.fmt, ",\n")
        });
        self
    }

    pub fn finish(&mut self) -> Result {
        self.result.and_then(|_| {
            self.pad.pad(&mut self.fmt)?;
            write!(self.fmt, "}}\n")
        })
    }
}
