// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=workgroup_memory_barrier::workgroup_memory_barrier
// normalize-stderr-test "OpLine .*\n" -> ""

use spirv_std::spirv;

unsafe fn workgroup_memory_barrier() {
    spirv_std::barrier::workgroup_memory_barrier();
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        workgroup_memory_barrier();
    }
}
