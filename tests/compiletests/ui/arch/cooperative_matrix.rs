// build-pass
// only-vulkan1.2
// compile-flags: -C target-feature=+CooperativeMatrixKHR,+ext:SPV_KHR_cooperative_matrix

use spirv_std::cooperative_matrix::{
    COLUMN_MAJOR, CooperativeMatrix, MATRIX_A, MATRIX_ACCUMULATOR, MATRIX_B, ROW_MAJOR, mul_add,
};
use spirv_std::spirv;

type MatA = CooperativeMatrix<f32, MATRIX_A, 16, 16>;
type MatB = CooperativeMatrix<f32, MATRIX_B, 16, 16>;
type MatAcc = CooperativeMatrix<f32, MATRIX_ACCUMULATOR, 16, 16>;

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] a: &[f32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] b: &[f32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 2)] c: &mut [f32],
) {
    let mat_a = unsafe { MatA::load(a, ROW_MAJOR, 16) };
    let mat_b = unsafe { MatB::load(b, COLUMN_MAJOR, 16) };
    let mat_c = unsafe { MatAcc::load(c, ROW_MAJOR, 16) };

    let result = mul_add(mat_a, mat_b, mat_c);

    unsafe { result.store(c, ROW_MAJOR, 16) };
}
