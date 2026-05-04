// Verifies that checked multiplication lowers to `OpUMulExtended` /
// `OpSMulExtended` and detects overflow correctly (issue #537).

// build-pass
// compile-flags: -C llvm-args=--disassemble-fn=checked_mul::checked_mul

use spirv_std::spirv;

fn checked_mul(a: u32, b: u32, c: i32, d: i32) -> u32 {
    let (ur, uo) = a.overflowing_mul(b);
    let (sr, so) = c.overflowing_mul(d);
    ur ^ (sr as u32) ^ (uo as u32) ^ (so as u32)
}

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] u_in: &[u32; 2],
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] s_in: &[i32; 2],
    out: &mut u32,
) {
    *out = checked_mul(u_in[0], u_in[1], s_in[0], s_in[1]);
}
