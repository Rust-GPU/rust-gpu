// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformVote,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_composite_all_equals::disassembly
// normalize-stderr-test "OpLine .*\n" -> ""

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

/// this should be 3 `subgroup_all_equal` instructions, with all calls inlined
fn disassembly(my_struct: MyStruct) -> bool {
    subgroup_all_equal(my_struct)
}

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(local_invocation_index)] inv_id: UVec3,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] output: &mut u32,
) {
    unsafe {
        let my_struct = MyStruct {
            a: inv_id.x as f32,
            b: inv_id,
            c: Nested(5i32 - inv_id.x as i32),
            d: Zst,
        };

        let bool = disassembly(my_struct);
        *output = u32::from(bool);
    }
}
