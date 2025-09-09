// Simple test demonstrating the errors from `fn` pointer / `dyn Trait` types.

// revisions: default spirt_diags
//[spirt_diags] compile-flags: -C llvm-args=--no-early-report-zombies

// build-fail

use core::ops::Range;
use spirv_std::spirv;

fn foo() {
    panic!("foo");
}
fn bar() {
    panic!("bar");
}

#[spirv(fragment)]
pub fn main_foo(out_fn: &mut fn(), out_dyn: &mut &'static dyn Iterator<Item = u32>) {
    *out_fn = foo;
    *out_dyn = &(0..10);
}

#[spirv(fragment)]
pub fn main_bar(out_fn: &mut fn(), out_dyn: &mut &'static dyn Fn(&mut Range<u32>) -> Option<u32>) {
    *out_fn = bar;
    *out_dyn = &|r| Some(r.end - r.start);
}
