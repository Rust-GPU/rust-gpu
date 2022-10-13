// build-pass
// compile-flags: -Ctarget-feature=+Geometry

#[rust_gpu::spirv(geometry(input_lines = 2, output_points = 2))]
pub fn main() {
    unsafe {
        spirv_std::arch::end_primitive();
    };
}
