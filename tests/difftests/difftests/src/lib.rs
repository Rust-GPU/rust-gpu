use std::borrow::Cow;
use std::fmt::Debug;

mod side;
mod fmt;

pub use side::*;
pub use fmt::*;

/// A [`Side`] of a difftest
pub struct Side<'a, T> {
    pub name: &'a str,
    pub value: T,
}

impl<'a, T> Side<'a, T> {
    pub fn new(name: &'a str, t: T) -> Self {
        Self { name, value: t }
    }
}

/// Run a difftest between these different [`Side`]s, panics on mismatch
pub fn difftest<T: Debug>(sides: &[Side<'_, T>]) {
    difftest_anyhow(sides).unwrap()
}

/// Run a difftest between these different [`Side`]s, returns an [`anyhow::Result`] instead of panic-ing
pub fn difftest_anyhow<T: Debug>(sides: &[Side<'_, T>]) -> anyhow::Result<()> {
    if sides.len() < 2 {
        anyhow::bail!("No trails compared, expected at least two trails");
    }
    let results = sides
        .into_iter()
        .map(|t| Side::new(t.name, format!("{:#?}", t.value)))
        .collect::<Vec<_>>();
    let reference = &results.first().unwrap().value;
    if results.iter().skip(1).all(|t| &t.value == reference) {
        // all trails are equal to each other
        Ok(())
    } else {
        // there was a mismatch between trails
        difftest_report(&results)
    }
}

/// Report a difference in any amount of trails
#[cold]
fn difftest_report(sides: &[Side<'_, String>]) -> anyhow::Result<()> {
    let mut hashed = HashMap::with_capacity(sides.len());
    for (i, side) in sides.iter().enumerate() {
        hashed
            .entry(side.value.as_str())
            .or_insert(Vec::new())
            .push(i);
    }

    let mut groups = hashed.into_iter().collect::<Vec<_>>();
    assert!(groups.len() >= 2);
    // sort with the most common group first
    // if there are multiple groups that are as common as the rest, sort them by *something* stable
    groups.sort_by(|a, b| a.1.len().cmp(&b.1.len()).then(a.0.cmp(b.0)));

    // the most common group serves as the reference
    let reference_ids = groups[0].1.as_slice();
    let reference = &sides[reference_ids[0]];

    anyhow::bail!(r#"Difftest failed!"#)
}
