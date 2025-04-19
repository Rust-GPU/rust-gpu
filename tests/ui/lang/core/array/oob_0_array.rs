// build-pass
// compile-flags: -C llvm-args=--disassemble-entry=compute
#![allow(unconditional_panic)]

#![cfg_attr(target_arch = "spirv", no_std)]
use spirv_std::spirv;

#[spirv(compute(threads(1, 1, 1)))]
pub fn compute() {
    let mut array = [0; 0];
    // writes to an array compile fine although they should be a panic.
    // (&mut array)[0] = 1; // this fails to compile, but it seems that below is
    // optimized out, and I'm not sure where.
    // GEP is not being hit, and neither is any load/store.
    array[0] = 1;
}
