#![no_std]

use spirv_std::arch::{atomic_i_add, atomic_i_sub, atomic_u_max, atomic_u_min};
use spirv_std::memory::{Scope, Semantics};
use spirv_std::spirv;

#[spirv(compute(threads(32)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] counters: &mut [u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] output: &mut [u32],
    #[spirv(global_invocation_id)] global_id: spirv_std::glam::UVec3,
) {
    const SCOPE: u32 = Scope::Workgroup as u32;
    const SEMANTICS: u32 = Semantics::NONE.bits();

    let tid = global_id.x;

    // All threads participate in atomic operations
    // Each thread adds 1 to the first counter
    unsafe { atomic_i_add::<_, SCOPE, SEMANTICS>(&mut counters[0], 1) };

    // Each thread subtracts 1 from the second counter
    unsafe { atomic_i_sub::<_, SCOPE, SEMANTICS>(&mut counters[1], 1) };

    // Each thread tries to set minimum with their thread ID
    unsafe { atomic_u_min::<_, SCOPE, SEMANTICS>(&mut counters[2], tid) };

    // Each thread tries to set maximum with their thread ID
    unsafe { atomic_u_max::<_, SCOPE, SEMANTICS>(&mut counters[3], tid) };

    // Thread 0 stores the final values after all operations complete
    if tid == 0 {
        // Use atomic loads to ensure we read the final values
        spirv_std::arch::workgroup_memory_barrier_with_group_sync();
        output[0] = counters[0]; // Should be initial + 32
        output[1] = counters[1]; // Should be initial - 32
        output[2] = counters[2]; // Should be min(initial, 0)
        output[3] = counters[3]; // Should be max(initial, 31)
        output[4] = counters[4]; // Unchanged
    }
}
