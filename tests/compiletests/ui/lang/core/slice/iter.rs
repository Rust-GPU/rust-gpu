// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=main

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] data: &[u32],
    output: &mut u32,
) {
    // NOTE(eddyb) can't use e.g. `.sum()` because that avoids the
    // "pointer range" `slice::Iter` behavior (using indices instead).
    for &x in data {
        *output += x;
    }
}
