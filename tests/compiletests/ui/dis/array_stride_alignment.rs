// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// CHECK: OpDecorate %{{[0-9]+}} ArrayStride 16

use spirv_std::spirv;

// Test that array stride respects alignment requirements
// vec3<f32> has size 12 bytes but alignment 16 bytes
// So array stride should be 16, not 12
#[derive(Copy, Clone)]
pub struct AlignedBuffer {
    data: [spirv_std::glam::Vec3; 4],
}

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] storage: &mut AlignedBuffer,
) {
    storage.data[0] = spirv_std::glam::Vec3::new(1.0, 2.0, 3.0);
}
