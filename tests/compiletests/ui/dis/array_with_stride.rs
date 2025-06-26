// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// CHECK: OpDecorate %{{[0-9]+}} ArrayStride

use spirv_std::spirv;

// Arrays in storage buffers should have ArrayStride decoration
#[derive(Copy, Clone)]
pub struct StorageBuffer {
    data: [f32; 4],
}

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] storage: &mut StorageBuffer,
) {
    storage.data[0] = 1.0;
    storage.data[1] = 2.0;
}
