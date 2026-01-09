// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformBallot,+GroupNonUniformShuffle,+GroupNonUniformShuffleRelative,+ext:SPV_KHR_vulkan_memory_model
// normalize-stderr-test "OpLine .*\n" -> ""
// ignore-vulkan1.0
// ignore-vulkan1.1
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-spv1.4

use glam::*;
use spirv_std::ScalarComposite;
use spirv_std::arch::*;
use spirv_std::spirv;

#[derive(Copy, Clone, ScalarComposite)]
pub struct MyStruct {
    a: f32,
    b: UVec3,
    c: Nested,
    d: Zst,
}

#[derive(Copy, Clone, ScalarComposite)]
pub struct Nested(i32);

#[derive(Copy, Clone, ScalarComposite)]
pub struct Zst;

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(local_invocation_id)] inv_id: UVec3,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] output: &mut UVec3,
) {
    unsafe {
        let my_struct = MyStruct {
            a: 1.,
            b: inv_id,
            c: Nested(-42),
            d: Zst,
        };

        let mut out = UVec3::ZERO;
        // before spv1.5 / vulkan1.2, this id = 19 must be a constant
        out += subgroup_broadcast(my_struct, 19).b;
        out += subgroup_broadcast_first(my_struct).b;
        out += subgroup_shuffle(my_struct, 2).b;
        out += subgroup_shuffle_xor(my_struct, 4).b;
        out += subgroup_shuffle_up(my_struct, 5).b;
        out += subgroup_shuffle_down(my_struct, 7).b;
        *output = out;
    }
}
