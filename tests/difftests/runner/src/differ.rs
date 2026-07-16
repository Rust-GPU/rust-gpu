#![allow(clippy::unimplemented)]

use difftest_types::config::OutputType;
use std::marker::PhantomData;

/// Represents the magnitude of a difference between two values
#[derive(Debug, Clone)]
pub enum DiffMagnitude {
    /// A numeric difference that can be measured
    Numeric(f64),
    /// A difference that cannot be measured numerically (e.g., raw bytes)
    Incomparable,
}

/// Trait for comparing two outputs and producing differences
pub trait OutputDiffer {
    /// Compare two outputs and return a list of differences
    fn compare(&self, output1: &[u8], output2: &[u8], epsilon: Option<f32>) -> Vec<Difference>;

    /// Get a human-readable name for this differ
    fn name(&self) -> &'static str;
}

/// Trait for displaying differences in various formats
pub trait DifferenceDisplay {
    /// Format differences as a table
    fn format_table(&self, diffs: &[Difference], pkg1: &str, pkg2: &str) -> String;

    /// Format differences as a detailed report
    fn format_report(
        &self,
        diffs: &[Difference],
        pkg1: &str,
        pkg2: &str,
        epsilon: Option<f32>,
    ) -> String;

    /// Write human-readable output to a file
    fn write_human_readable(&self, output: &[u8], path: &std::path::Path) -> std::io::Result<()>;
}

/// A single difference between two values
#[derive(Debug, Clone)]
pub struct Difference {
    pub index: usize,
    pub value1: String,
    pub value2: String,
    pub absolute_diff: DiffMagnitude,
    pub relative_diff: DiffMagnitude,
}

/// Differ for raw byte comparison
pub struct RawDiffer;

impl OutputDiffer for RawDiffer {
    fn compare(&self, output1: &[u8], output2: &[u8], _epsilon: Option<f32>) -> Vec<Difference> {
        if output1 == output2 {
            vec![]
        } else {
            let mut differences = Vec::new();
            let max_len = std::cmp::max(output1.len(), output2.len());

            // Find byte-level differences
            for i in 0..max_len {
                let byte1 = output1.get(i);
                let byte2 = output2.get(i);

                match (byte1, byte2) {
                    (Some(&b1), Some(&b2)) if b1 != b2 => {
                        differences.push(Difference {
                            index: i,
                            value1: format!("{b1}"),
                            value2: format!("{b2}"),
                            absolute_diff: DiffMagnitude::Incomparable,
                            relative_diff: DiffMagnitude::Incomparable,
                        });
                    }
                    (Some(&b1), None) => {
                        differences.push(Difference {
                            index: i,
                            value1: format!("{b1}"),
                            value2: "".to_string(),
                            absolute_diff: DiffMagnitude::Incomparable,
                            relative_diff: DiffMagnitude::Incomparable,
                        });
                    }
                    (None, Some(&b2)) => {
                        differences.push(Difference {
                            index: i,
                            value1: "".to_string(),
                            value2: format!("{b2}"),
                            absolute_diff: DiffMagnitude::Incomparable,
                            relative_diff: DiffMagnitude::Incomparable,
                        });
                    }
                    _ => {} // bytes are equal
                }
            }

            differences
        }
    }

    fn name(&self) -> &'static str {
        "Raw Binary"
    }
}

impl DifferenceDisplay for RawDiffer {
    fn format_table(&self, diffs: &[Difference], pkg1: &str, pkg2: &str) -> String {
        use tabled::settings::{Alignment, Modify, Span, Style, object::Rows};

        let rows: Vec<Vec<String>> = diffs
            .iter()
            .take(10)
            .map(|d| {
                let (hex1, dec1, ascii1) = if d.value1.is_empty() {
                    ("--".to_string(), "--".to_string(), "--".to_string())
                } else {
                    let byte = d.value1.parse::<u8>().unwrap();
                    let ascii = if byte.is_ascii_graphic() || byte == b' ' {
                        format!("{}", byte as char)
                    } else {
                        match byte {
                            b'\n' => "\\n".to_string(),
                            b'\r' => "\\r".to_string(),
                            b'\t' => "\\t".to_string(),
                            b'\0' => "\\0".to_string(),
                            _ => "".to_string(), // Empty for non-printable
                        }
                    };
                    (
                        format!("{:>3}", format!("{:02x}", byte)),
                        format!("{byte:3}"),
                        format!("{ascii:^5}"),
                    )
                };

                let (hex2, dec2, ascii2) = if d.value2.is_empty() {
                    ("--".to_string(), "--".to_string(), "--".to_string())
                } else {
                    let byte = d.value2.parse::<u8>().unwrap();
                    let ascii = if byte.is_ascii_graphic() || byte == b' ' {
                        format!("{}", byte as char)
                    } else {
                        match byte {
                            b'\n' => "\\n".to_string(),
                            b'\r' => "\\r".to_string(),
                            b'\t' => "\\t".to_string(),
                            b'\0' => "\\0".to_string(),
                            _ => "".to_string(), // Empty for non-printable
                        }
                    };
                    (
                        format!("{:>3}", format!("{:02x}", byte)),
                        format!("{byte:3}"),
                        format!("{ascii:^5}"),
                    )
                };

                vec![
                    format!("0x{:04x}", d.index),
                    hex1,
                    dec1,
                    ascii1,
                    hex2,
                    dec2,
                    ascii2,
                ]
            })
            .collect();

        let mut builder = tabled::builder::Builder::default();

        // Header rows
        builder.push_record(vec!["Offset", pkg1, "", "", pkg2, "", ""]);
        builder.push_record(vec!["", "Hex", "Dec", "ASCII", "Hex", "Dec", "ASCII"]);

        for row in &rows {
            builder.push_record(row);
        }

        let mut table = builder.build();
        table
            .with(Style::modern())
            .with(Modify::new(Rows::new(0..)).with(Alignment::center()))
            // Apply column spans to merge the package names across their columns
            .modify((0, 1), Span::column(3))
            .modify((0, 4), Span::column(3))
            // Remove the borders between merged cells
            .with(tabled::settings::themes::BorderCorrection::span());

        let mut result = table.to_string();

        if diffs.len() > 10 {
            let last_line_width = result.lines().last().map_or(0, |l| l.chars().count());
            result.push_str(&format!(
                "\n{:>width$}",
                format!("... {} more differences", diffs.len() - 10),
                width = last_line_width
            ));
        }

        result
    }

    fn format_report(
        &self,
        diffs: &[Difference],
        pkg1: &str,
        pkg2: &str,
        _epsilon: Option<f32>,
    ) -> String {
        let mut report = format!("Total differences: {} bytes\n\n", diffs.len());
        report.push_str(&self.format_table(diffs, pkg1, pkg2));

        report
    }

    fn write_human_readable(&self, output: &[u8], path: &std::path::Path) -> std::io::Result<()> {
        // For raw binary, write hex dump
        use std::io::Write;
        let mut file = std::fs::File::create(path)?;
        for (i, chunk) in output.chunks(16).enumerate() {
            write!(file, "{:08x}: ", i * 16)?;
            for byte in chunk {
                write!(file, "{byte:02x} ")?;
            }
            writeln!(file)?;
        }
        Ok(())
    }
}

/// Trait for numeric types that can be diffed
pub trait NumericType: Copy + PartialEq + std::fmt::Display + Send + Sync + 'static {
    fn from_bytes(bytes: &[u8]) -> Self;
    fn abs_diff(a: Self, b: Self) -> f64;
    fn type_name() -> &'static str;
    fn format_value(value: Self) -> String;
    fn can_have_relative_diff() -> bool;
    fn as_f64(value: Self) -> f64;
}

impl NumericType for f32 {
    fn from_bytes(bytes: &[u8]) -> Self {
        f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
    }
    fn abs_diff(a: Self, b: Self) -> f64 {
        (a - b).abs() as f64
    }
    fn type_name() -> &'static str {
        "F32"
    }
    fn format_value(value: Self) -> String {
        format!("{value:.9}")
    }
    fn can_have_relative_diff() -> bool {
        true
    }
    fn as_f64(value: Self) -> f64 {
        value as f64
    }
}

impl NumericType for u32 {
    fn from_bytes(bytes: &[u8]) -> Self {
        u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
    }
    fn abs_diff(a: Self, b: Self) -> f64 {
        if a > b {
            (a - b) as f64
        } else {
            (b - a) as f64
        }
    }
    fn type_name() -> &'static str {
        "U32"
    }
    fn format_value(value: Self) -> String {
        format!("{value}")
    }
    fn can_have_relative_diff() -> bool {
        true
    }
    fn as_f64(value: Self) -> f64 {
        value as f64
    }
}

/// Generic differ for numeric types
pub struct NumericDiffer<T: NumericType> {
    _phantom: PhantomData<T>,
}

impl<T: NumericType> Default for NumericDiffer<T> {
    fn default() -> Self {
        Self {
            _phantom: PhantomData,
        }
    }
}

impl<T: NumericType> OutputDiffer for NumericDiffer<T> {
    fn compare(&self, output1: &[u8], output2: &[u8], epsilon: Option<f32>) -> Vec<Difference> {
        if output1.len() != output2.len() {
            return vec![Difference {
                index: 0,
                value1: format!("{} bytes", output1.len()),
                value2: format!("{} bytes", output2.len()),
                absolute_diff: DiffMagnitude::Numeric(0.0),
                relative_diff: DiffMagnitude::Incomparable,
            }];
        }

        let values1: Vec<T> = output1
            .chunks_exact(std::mem::size_of::<T>())
            .map(T::from_bytes)
            .collect();
        let values2: Vec<T> = output2
            .chunks_exact(std::mem::size_of::<T>())
            .map(T::from_bytes)
            .collect();

        let mut differences = Vec::new();
        for (i, (&v1, &v2)) in values1.iter().zip(values2.iter()).enumerate() {
            if v1 != v2 {
                let diff = T::abs_diff(v1, v2);
                // For floating point types, check epsilon threshold
                let threshold = if T::can_have_relative_diff() {
                    epsilon.unwrap_or(0.0) as f64
                } else {
                    0.0
                };

                if diff > threshold {
                    let rel_diff = if T::can_have_relative_diff() {
                        let v1_abs = T::as_f64(v1).abs();
                        let v2_abs = T::as_f64(v2).abs();
                        let max_abs = f64::max(v1_abs, v2_abs);
                        if max_abs > 1e-10 {
                            DiffMagnitude::Numeric(diff / max_abs)
                        } else {
                            DiffMagnitude::Incomparable
                        }
                    } else {
                        DiffMagnitude::Incomparable
                    };

                    differences.push(Difference {
                        index: i,
                        value1: T::format_value(v1),
                        value2: T::format_value(v2),
                        absolute_diff: DiffMagnitude::Numeric(diff),
                        relative_diff: rel_diff,
                    });
                }
            }
        }

        differences
    }

    fn name(&self) -> &'static str {
        T::type_name()
    }
}

impl<T: NumericType> DifferenceDisplay for NumericDiffer<T> {
    fn format_table(&self, diffs: &[Difference], pkg1: &str, pkg2: &str) -> String {
        use tabled::settings::{Alignment, Modify, Style, object::Rows};

        let rows: Vec<Vec<String>> = diffs
            .iter()
            .take(10)
            .map(|d| {
                let abs_str = match &d.absolute_diff {
                    DiffMagnitude::Numeric(val) => {
                        if T::can_have_relative_diff() {
                            format!("{val:.3e}")
                        } else {
                            format!("{}", *val as u64)
                        }
                    }
                    DiffMagnitude::Incomparable => "N/A".to_string(),
                };

                let rel_str = match &d.relative_diff {
                    DiffMagnitude::Numeric(val) => format!("{:.2}%", val * 100.0),
                    DiffMagnitude::Incomparable => "N/A".to_string(),
                };

                if T::can_have_relative_diff() {
                    vec![
                        d.index.to_string(),
                        d.value1.clone(),
                        d.value2.clone(),
                        abs_str,
                        rel_str,
                    ]
                } else {
                    vec![
                        d.index.to_string(),
                        d.value1.clone(),
                        d.value2.clone(),
                        abs_str,
                    ]
                }
            })
            .collect();

        let mut builder = tabled::builder::Builder::default();

        if T::can_have_relative_diff() {
            builder.push_record(vec!["#", pkg1, pkg2, "Δ abs", "Δ %"]);
        } else {
            builder.push_record(vec!["#", pkg1, pkg2, "Δ"]);
        }

        for row in &rows {
            builder.push_record(row);
        }

        let mut table = builder.build();
        table
            .with(Style::modern())
            .with(Modify::new(Rows::first()).with(Alignment::center()));

        let mut result = table.to_string();

        if diffs.len() > 10 {
            let last_line_width = result.lines().last().map_or(0, |l| l.chars().count());
            result.push_str(&format!(
                "\n{:>width$}",
                format!("... {} more differences", diffs.len() - 10),
                width = last_line_width
            ));
        }

        result
    }

    fn format_report(
        &self,
        diffs: &[Difference],
        pkg1: &str,
        pkg2: &str,
        _epsilon: Option<f32>,
    ) -> String {
        self.format_table(diffs, pkg1, pkg2)
    }

    fn write_human_readable(&self, output: &[u8], path: &std::path::Path) -> std::io::Result<()> {
        use std::io::Write;
        let mut file = std::fs::File::create(path)?;

        let values: Vec<T> = output
            .chunks_exact(std::mem::size_of::<T>())
            .map(T::from_bytes)
            .collect();

        for (i, &value) in values.iter().enumerate() {
            writeln!(file, "{}: {}", i, T::format_value(value))?;
        }

        Ok(())
    }
}

/// Type aliases for specific numeric differs
pub type F32Differ = NumericDiffer<f32>;
pub type U32Differ = NumericDiffer<u32>;

impl From<OutputType> for Box<dyn OutputDiffer + Send + Sync> {
    fn from(output_type: OutputType) -> Self {
        match output_type {
            OutputType::Raw => Box::new(RawDiffer),
            OutputType::F32 => Box::new(F32Differ::default()),
            OutputType::F64 => unimplemented!("F64Differ not implemented yet"),
            OutputType::U32 => Box::new(U32Differ::default()),
            OutputType::I32 => unimplemented!("I32Differ not implemented yet"),
        }
    }
}

impl From<OutputType> for Box<dyn DifferenceDisplay + Send + Sync> {
    fn from(output_type: OutputType) -> Self {
        match output_type {
            OutputType::Raw => Box::new(RawDiffer),
            OutputType::F32 => Box::new(F32Differ::default()),
            OutputType::F64 => unimplemented!("F64Differ not implemented yet"),
            OutputType::U32 => Box::new(U32Differ::default()),
            OutputType::I32 => unimplemented!("I32Differ not implemented yet"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_u32_differ_no_differences() {
        let differ = U32Differ::default();
        let data1 = vec![1u32, 2, 3, 4];
        let bytes1 = bytemuck::cast_slice(&data1);
        let bytes2 = bytemuck::cast_slice(&data1);

        let diffs = differ.compare(bytes1, bytes2, None);
        assert!(diffs.is_empty());
    }

    #[test]
    fn test_u32_differ_with_differences() {
        let differ = U32Differ::default();
        let data1 = vec![1u32, 2, 3, 4];
        let data2 = vec![1u32, 5, 3, 7];
        let bytes1 = bytemuck::cast_slice(&data1);
        let bytes2 = bytemuck::cast_slice(&data2);

        let diffs = differ.compare(bytes1, bytes2, None);
        assert_eq!(diffs.len(), 2);

        // Check first difference (index 1: 2 vs 5)
        assert_eq!(diffs[0].index, 1);
        assert_eq!(diffs[0].value1, "2");
        assert_eq!(diffs[0].value2, "5");
        match &diffs[0].absolute_diff {
            DiffMagnitude::Numeric(val) => assert_eq!(*val, 3.0),
            DiffMagnitude::Incomparable => panic!("Expected numeric difference"),
        }
        match &diffs[0].relative_diff {
            DiffMagnitude::Numeric(val) => assert_eq!(*val, 3.0 / 5.0), // 3/5 = 0.6
            DiffMagnitude::Incomparable => panic!("Expected numeric relative diff for U32"),
        }

        // Check second difference (index 3: 4 vs 7)
        assert_eq!(diffs[1].index, 3);
        assert_eq!(diffs[1].value1, "4");
        assert_eq!(diffs[1].value2, "7");
        match &diffs[1].absolute_diff {
            DiffMagnitude::Numeric(val) => assert_eq!(*val, 3.0),
            DiffMagnitude::Incomparable => panic!("Expected numeric difference"),
        }
    }

    #[test]
    fn test_u32_differ_different_lengths() {
        let differ = U32Differ::default();
        let data1 = vec![1u32, 2];
        let data2 = vec![1u32, 2, 3, 4];
        let bytes1 = bytemuck::cast_slice(&data1);
        let bytes2 = bytemuck::cast_slice(&data2);

        let diffs = differ.compare(bytes1, bytes2, None);
        assert_eq!(diffs.len(), 1);
        assert_eq!(diffs[0].value1, "8 bytes");
        assert_eq!(diffs[0].value2, "16 bytes");
    }

    #[test]
    fn test_f32_differ_with_epsilon() {
        let differ = F32Differ::default();
        let data1 = vec![1.0f32, 2.0, 3.0];
        let data2 = vec![1.0001f32, 2.0, 3.01];
        let bytes1 = bytemuck::cast_slice(&data1);
        let bytes2 = bytemuck::cast_slice(&data2);

        // With epsilon = 0.001, only the third value should be reported
        let diffs = differ.compare(bytes1, bytes2, Some(0.001));
        assert_eq!(diffs.len(), 1);
        assert_eq!(diffs[0].index, 2);

        // Without epsilon, both differences should be reported
        let diffs = differ.compare(bytes1, bytes2, None);
        assert_eq!(diffs.len(), 2);
    }

    #[test]
    fn test_raw_differ() {
        let differ = RawDiffer;
        let bytes1 = b"hello";
        let bytes2 = b"world";

        let diffs = differ.compare(bytes1, bytes2, None);
        assert_eq!(diffs.len(), 4); // 4 bytes differ (l at position 3 is same in both)

        // Check first difference (h vs w)
        assert_eq!(diffs[0].index, 0);
        assert_eq!(diffs[0].value1, "104"); // h = 104
        assert_eq!(diffs[0].value2, "119"); // w = 119

        // Check second difference (e vs o)
        assert_eq!(diffs[1].index, 1);
        assert_eq!(diffs[1].value1, "101"); // 'e' = 101
        assert_eq!(diffs[1].value2, "111"); // 'o' = 111

        // Check third difference (first l vs r)
        assert_eq!(diffs[2].index, 2);
        assert_eq!(diffs[2].value1, "108"); // 'l' = 108
        assert_eq!(diffs[2].value2, "114"); // 'r' = 114

        // Check fourth difference (o vs d)
        assert_eq!(diffs[3].index, 4);
        assert_eq!(diffs[3].value1, "111"); // 'o' = 111
        assert_eq!(diffs[3].value2, "100"); // 'd' = 100
    }

    #[test]
    fn test_raw_differ_partial_match() {
        let differ = RawDiffer;
        let bytes1 = b"hello world";
        let bytes2 = b"hello earth";

        let diffs = differ.compare(bytes1, bytes2, None);
        assert_eq!(diffs.len(), 4); // 4 bytes differ in "world" vs "earth" (r at position 8 is same)

        // First difference should be at index 6 (w vs e)
        assert_eq!(diffs[0].index, 6);
        assert_eq!(diffs[0].value1, "119"); // 'w' = 119
        assert_eq!(diffs[0].value2, "101"); // 'e' = 101

        // Second difference at index 7 (o vs a)
        assert_eq!(diffs[1].index, 7);
        assert_eq!(diffs[1].value1, "111"); // 'o' = 111
        assert_eq!(diffs[1].value2, "97"); // 'a' = 97

        // Third difference at index 9 (l vs t)
        assert_eq!(diffs[2].index, 9);
        assert_eq!(diffs[2].value1, "108"); // 'l' = 108
        assert_eq!(diffs[2].value2, "116"); // 't' = 116

        // Fourth difference at index 10 (d vs h)
        assert_eq!(diffs[3].index, 10);
        assert_eq!(diffs[3].value1, "100"); // 'd' = 100
        assert_eq!(diffs[3].value2, "104"); // 'h' = 104
    }

    #[test]
    fn test_raw_differ_different_lengths() {
        let differ = RawDiffer;
        let bytes1 = b"hello";
        let bytes2 = b"hello world";

        let diffs = differ.compare(bytes1, bytes2, None);
        assert_eq!(diffs.len(), 6); // " world" = 6 extra bytes

        // Check that missing bytes are shown as empty string
        assert_eq!(diffs[0].index, 5);
        assert_eq!(diffs[0].value1, "");
        assert_eq!(diffs[0].value2, "32"); // ' ' = 32
    }

    #[test]
    fn test_diff_magnitude_enum() {
        // Test that we can create and match on DiffMagnitude variants
        let numeric = DiffMagnitude::Numeric(42.0);
        let incomparable = DiffMagnitude::Incomparable;

        match numeric {
            DiffMagnitude::Numeric(val) => assert_eq!(val, 42.0),
            DiffMagnitude::Incomparable => panic!("Expected numeric"),
        }

        match incomparable {
            DiffMagnitude::Incomparable => {}
            DiffMagnitude::Numeric(_) => panic!("Expected incomparable"),
        }
    }
}
