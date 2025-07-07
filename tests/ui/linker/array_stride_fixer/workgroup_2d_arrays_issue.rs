// Test that reproduces the OpInBoundsAccessChain type mismatch issue
// with workgroup 2D arrays after array_stride_fixer changes

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// only-vulkan1.1
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "\S*/lib/rustlib/" -> "$SYSROOT/lib/rustlib/"

use spirv_std::spirv;

const TILE_SIZE: u32 = 32;

#[spirv(compute(threads(32, 32)))]
pub fn transpose_2d_workgroup(
    #[spirv(local_invocation_id)] lid: spirv_std::glam::UVec3,
    #[spirv(workgroup)] shared_real: &mut [[f32; TILE_SIZE as usize]; TILE_SIZE as usize],
    #[spirv(workgroup)] shared_imag: &mut [[f32; TILE_SIZE as usize]; TILE_SIZE as usize],
) {
    let lx = lid.x as usize;
    let ly = lid.y as usize;

    // This should trigger the OpInBoundsAccessChain issue
    shared_real[ly][lx] = 1.0;
    shared_imag[ly][lx] = 2.0;

    // Read back to ensure usage
    let _val = shared_real[lx][ly] + shared_imag[lx][ly];
}
