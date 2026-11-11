// Test that `spirv_std::ptr::PhysicalPtr` requires `PhysicalStorageBufferAddresses`.

// build-fail
// compile-flags: -C target-feature=+Int64,+ext:SPV_KHR_physical_storage_buffer

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
    if !input.is_null() {
        unsafe {
            *output.as_mut_unchecked() = *input.as_ref_unchecked();
        }
    }
}
