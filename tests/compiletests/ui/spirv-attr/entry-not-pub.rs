// Tests that `#[spirv(...)]` entry-point functions don't need to be declared
// `pub` — the macro adds it automatically.

// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
fn main() {}
