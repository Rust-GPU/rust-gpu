// built-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformArithmetic,+ext:SPV_KHR_vulkan_memory_model

use spirv_std::glam::*;
use spirv_std::spirv;

// assumption: subgroup size == 32
#[spirv(compute(threads(128)))]
pub fn sum(
    #[spirv(global_invocation_id)] global_invocation_id: UVec3,
    #[spirv(local_invocation_id)] local_invocation_id: UVec3,
    #[spirv(num_workgroups)] num_workgroups: UVec3,
    #[spirv(workgroup_id)] workgroup_id: UVec3,
    #[spirv(workgroup)] shared_memory: &mut [f32; 4],
    #[spirv(spec_constant(id = 0, default = 42))] spec_constant: u32,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] values: &[f32],
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] output: &mut f32,
) {
    let value = values[global_invocation_id.x as usize];
    let sum_per_subgroup = spirv_std::arch::subgroup_f_add(value);

    let local_idx = local_invocation_id.x as usize;
    if local_idx % 32 == 0 {
        shared_memory[local_idx / 32] = sum_per_subgroup;
    }

    spirv_std::arch::workgroup_memory_barrier_with_group_sync();
    if local_idx == 0 {
        let mut accum = 0.;
        for i in 0..4 {
            shared_memory[i] += accum;
        }
        *output = accum;
    }
}
