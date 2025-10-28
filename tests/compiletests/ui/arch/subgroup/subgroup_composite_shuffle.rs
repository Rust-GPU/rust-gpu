// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformShuffle,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_composite_shuffle::disassembly
// normalize-stderr-test "OpLine .*\n" -> ""

use glam::*;
use spirv_std::ScalarOrVectorComposite;
use spirv_std::arch::*;
use spirv_std::spirv;

#[derive(Copy, Clone, ScalarOrVectorComposite)]
pub struct MyStruct {
    a: f32,
    b: UVec3,
    c: Nested,
    d: Zst,
}

#[derive(Copy, Clone, ScalarOrVectorComposite)]
pub struct Nested(i32);

#[derive(Copy, Clone, ScalarOrVectorComposite)]
pub struct Zst;

/// this should be 3 `subgroup_shuffle` instructions, with all calls inlined
fn disassembly(my_struct: MyStruct, id: u32) -> MyStruct {
    subgroup_shuffle(my_struct, id)
}

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(local_invocation_index)] inv_id: UVec3,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] output: &mut MyStruct,
) {
    unsafe {
        let my_struct = MyStruct {
            a: inv_id.x as f32,
            b: inv_id,
            c: Nested(5i32 - inv_id.x as i32),
            d: Zst,
        };

        *output = disassembly(my_struct, 5);
    }
}
