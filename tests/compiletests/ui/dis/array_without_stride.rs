// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// CHECK-NOT: ArrayStride

use spirv_std::spirv;

// Arrays containing pointers should NOT have ArrayStride decoration
// This tests the case from issue #266 where arrays in function/private storage
// shouldn't have explicit layout decorations

// Use a struct with an array of pointers to ensure the array type is preserved
#[derive(Copy, Clone)]
pub struct WorkgroupData {
    pointers: [*mut f32; 4],
}

#[spirv(compute(threads(1)))]
pub fn main_cs(#[spirv(workgroup)] shared: &mut WorkgroupData) {
    // Just read the pointer array to ensure it's used
    let _first_ptr = shared.pointers[0];
}
