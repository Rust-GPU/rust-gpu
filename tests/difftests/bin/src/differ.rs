use difftest::config::OutputType;

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
    pub absolute_diff: f64,
    pub relative_diff: Option<f64>,
}

/// Differ for raw byte comparison
pub struct RawDiffer;

impl OutputDiffer for RawDiffer {
    fn compare(&self, output1: &[u8], output2: &[u8], _epsilon: Option<f32>) -> Vec<Difference> {
        if output1 == output2 {
            vec![]
        } else {
            // For raw comparison, we just note that they differ
            vec![Difference {
                index: 0,
                value1: format!("{} bytes", output1.len()),
                value2: format!("{} bytes", output2.len()),
                absolute_diff: 0.0,
                relative_diff: None,
            }]
        }
    }

    fn name(&self) -> &'static str {
        "Raw Binary"
    }
}

impl DifferenceDisplay for RawDiffer {
    fn format_table(&self, _diffs: &[Difference], _pkg1: &str, _pkg2: &str) -> String {
        "Binary files differ".to_string()
    }

    fn format_report(
        &self,
        _diffs: &[Difference],
        pkg1: &str,
        pkg2: &str,
        _epsilon: Option<f32>,
    ) -> String {
        format!("Binary outputs from {} and {} differ", pkg1, pkg2)
    }

    fn write_human_readable(&self, output: &[u8], path: &std::path::Path) -> std::io::Result<()> {
        // For raw binary, write hex dump
        use std::io::Write;
        let mut file = std::fs::File::create(path)?;
        for (i, chunk) in output.chunks(16).enumerate() {
            write!(file, "{:08x}: ", i * 16)?;
            for byte in chunk {
                write!(file, "{:02x} ", byte)?;
            }
            writeln!(file)?;
        }
        Ok(())
    }
}

/// Differ for f32 arrays
pub struct F32Differ;

impl OutputDiffer for F32Differ {
    fn compare(&self, output1: &[u8], output2: &[u8], epsilon: Option<f32>) -> Vec<Difference> {
        if output1.len() != output2.len() {
            return vec![Difference {
                index: 0,
                value1: format!("{} bytes", output1.len()),
                value2: format!("{} bytes", output2.len()),
                absolute_diff: 0.0,
                relative_diff: None,
            }];
        }

        let floats1: Vec<f32> = output1
            .chunks_exact(4)
            .map(|chunk| f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
            .collect();
        let floats2: Vec<f32> = output2
            .chunks_exact(4)
            .map(|chunk| f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
            .collect();

        let mut differences = Vec::new();

        for (i, (&v1, &v2)) in floats1.iter().zip(floats2.iter()).enumerate() {
            let diff = (v1 - v2).abs();
            // If no epsilon specified, report all differences
            let threshold = epsilon.unwrap_or(0.0);
            if diff > threshold {
                let rel_diff = if v1.abs() > 1e-10 || v2.abs() > 1e-10 {
                    Some((diff as f64) / f64::max(v1.abs() as f64, v2.abs() as f64))
                } else {
                    None
                };

                differences.push(Difference {
                    index: i,
                    value1: format!("{:.9}", v1),
                    value2: format!("{:.9}", v2),
                    absolute_diff: diff as f64,
                    relative_diff: rel_diff,
                });
            }
        }

        differences
    }

    fn name(&self) -> &'static str {
        "32-bit Float"
    }
}

impl DifferenceDisplay for F32Differ {
    fn format_table(&self, diffs: &[Difference], pkg1: &str, pkg2: &str) -> String {
        use tabled::settings::{Alignment, Modify, Style, object::Rows};

        if diffs.is_empty() {
            return String::new();
        }

        // Extract suffix from package names (e.g., "math_ops-wgsl" -> "WGSL")
        let extract_suffix = |pkg: &str| -> String {
            if let Some(pos) = pkg.rfind('-') {
                let suffix = &pkg[pos + 1..];
                suffix.to_uppercase()
            } else {
                pkg.to_string()
            }
        };

        let col1_name = extract_suffix(pkg1);
        let col2_name = extract_suffix(pkg2);

        // Build rows for the table
        let rows: Vec<Vec<String>> = diffs
            .iter()
            .take(10)
            .map(|d| {
                vec![
                    d.index.to_string(),
                    d.value1.clone(),
                    d.value2.clone(),
                    format!("{:.3e}", d.absolute_diff),
                    d.relative_diff
                        .map(|r| format!("{:.2}%", r * 100.0))
                        .unwrap_or_else(|| "N/A".to_string()),
                ]
            })
            .collect();

        // Create a table with custom headers
        let mut builder = tabled::builder::Builder::default();

        // Add header row
        builder.push_record(vec!["#", &col1_name, &col2_name, "Δ abs", "Δ %"]);

        // Add data rows
        for row in &rows {
            builder.push_record(row);
        }

        let mut table = builder.build();
        table
            .with(Style::modern())
            .with(Modify::new(Rows::first()).with(Alignment::center()));

        let mut result = table.to_string();

        if diffs.len() > 10 {
            // Get the width of the last line to properly align the text
            let last_line_width = result
                .lines()
                .last()
                .map(|l| l.chars().count())
                .unwrap_or(0);
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

        let floats: Vec<f32> = output
            .chunks_exact(4)
            .map(|chunk| f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
            .collect();

        for (i, &value) in floats.iter().enumerate() {
            writeln!(file, "{}: {:.9}", i, value)?;
        }

        Ok(())
    }
}

impl From<OutputType> for Box<dyn OutputDiffer + Send + Sync> {
    fn from(output_type: OutputType) -> Self {
        match output_type {
            OutputType::Raw => Box::new(RawDiffer),
            OutputType::F32 => Box::new(F32Differ),
            OutputType::F64 => todo!("F64Differ not implemented yet"),
            OutputType::U32 => todo!("U32Differ not implemented yet"),
            OutputType::I32 => todo!("I32Differ not implemented yet"),
        }
    }
}

impl From<OutputType> for Box<dyn DifferenceDisplay + Send + Sync> {
    fn from(output_type: OutputType) -> Self {
        match output_type {
            OutputType::Raw => Box::new(RawDiffer),
            OutputType::F32 => Box::new(F32Differ),
            OutputType::F64 => todo!("F64Display not implemented yet"),
            OutputType::U32 => todo!("U32Display not implemented yet"),
            OutputType::I32 => todo!("I32Display not implemented yet"),
        }
    }
}
