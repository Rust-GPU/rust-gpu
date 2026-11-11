// Test reads and writes through `spirv_std::ptr::PhysicalPtr`.

// build-pass
// compile-flags: -C target-feature=+PhysicalStorageBufferAddresses,+Int64,+ext:SPV_KHR_physical_storage_buffer
// compile-flags: -C llvm-args=--disassemble-entry=main
// normalize-stderr-test "\n\W*OpLine .*" -> ""

use spirv_std::ptr::PhysicalPtr;
use spirv_std::spirv;

#[repr(C)]
pub struct Buffers {
    input: PhysicalPtr<u32>,
    output: PhysicalPtr<u32>,
}

#[spirv(compute(threads(1)))]
pub fn main(#[spirv(push_constant)] buffers: &Buffers) {
    let (input, output) = (buffers.input, buffers.output);
    if !input.is_null() && !output.is_null() {
        unsafe {
            let x = *input.as_ref_unchecked() + *input.add(1).as_ref_unchecked();
            *output.as_mut_unchecked() = x;
            *output.add(1).cast::<f32>().as_mut_unchecked() = x as f32;
        }
    }
}
