// build-pass

use spirv_std::arch::IndexUnchecked;

#[rust_gpu::spirv(fragment)]
pub fn main(
    #[rust_gpu::spirv(descriptor_set = 0, binding = 0, storage_buffer)] runtime_array: &mut [u32],
    #[rust_gpu::spirv(descriptor_set = 1, binding = 1, storage_buffer)] array: &mut [u32; 5],
) {
    unsafe {
        *runtime_array.index_unchecked_mut(0) = *array.index_unchecked(0);
        *array.index_unchecked_mut(1) = *runtime_array.index_unchecked(1);
    }
}
