//! Code for running the Collatz problem on the CPU. See the wgpu compute runner
//! for the equivalent GPU code.
//!
//! Currently it's not actually built or tested, and is just here for reference.

use std::time::Instant;

use compute_shader::collatz;
use rayon::prelude::*;

fn main() {
    let top = 2u32.pow(20);
    let src_range = 1..top;
    let start = Instant::now();
    let result = src_range
        .clone()
        .into_par_iter()
        .map(collatz)
        .collect::<Vec<_>>();
    let took = start.elapsed();
    println!("1: 0");
    let mut max = 0;
    for (src, out) in src_range.zip(result.iter().copied()) {
        match out {
            Some(out) if out > max => {
                max = out;
                println!("{src}: {out}");
            }
            Some(_) => {}
            None => {
                println!("{src}: overflowed");
                break;
            }
        }
    }
    println!("Took: {took:?}");
}
