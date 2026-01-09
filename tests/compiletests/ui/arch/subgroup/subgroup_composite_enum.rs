// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+GroupNonUniformShuffle,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_composite_enum::disassembly
// normalize-stderr-test "OpLine .*\n" -> ""

use glam::*;
use spirv_std::ScalarComposite;
use spirv_std::arch::*;
use spirv_std::spirv;

#[repr(u32)]
#[derive(Copy, Clone, Default, ScalarComposite)]
pub enum MyEnum {
    #[default]
    A,
    B,
    C,
}

impl From<u32> for MyEnum {
    #[inline]
    fn from(value: u32) -> Self {
        match value {
            0 => Self::A,
            1 => Self::B,
            2 => Self::C,
            _ => Self::default(),
        }
    }
}

impl From<MyEnum> for u32 {
    #[inline]
    fn from(value: MyEnum) -> Self {
        value as u32
    }
}

/// this should be a single `subgroup_shuffle` instruction, with all calls inlined
fn disassembly(my_struct: MyEnum, id: u32) -> MyEnum {
    subgroup_shuffle(my_struct, id)
}

#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(local_invocation_index)] inv_id: u32,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] output: &mut MyEnum,
) {
    unsafe {
        let my_enum = MyEnum::from(inv_id % 3);
        *output = disassembly(my_enum, 5);
    }
}
