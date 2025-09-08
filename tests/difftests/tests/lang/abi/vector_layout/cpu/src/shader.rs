use crate::layout::eval_layouts;
use spirv_std::glam::UVec3;
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(workgroup_id)] gid: UVec3,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut [u32],
) {
    eval_layouts(gid.x, output);
}
