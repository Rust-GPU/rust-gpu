// build-pass
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpLine .*\n" -> ""
// normalize-stderr-test "%\d+ = OpString .*\n" -> ""
// normalize-stderr-test "; .*\n" -> ""
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"
// ignore-vulkan1.0
// ignore-vulkan1.1

use spirv_std::arch::subgroup_shuffle_up;
use spirv_std::spirv;

#[repr(C)]
#[derive(Copy, Clone, Default)]
#[rust_gpu::vector::v1]
pub struct Vec3 {
    x: f32,
    y: f32,
    z: f32,
}

#[repr(C, align(16))]
#[derive(Copy, Clone, Default)]
#[rust_gpu::vector::v1]
pub struct Vec3A {
    x: f32,
    y: f32,
    z: f32,
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct Data<T> {
    t: T,
    // this should generate two distinct structs where this member has different offsets
    value: f32,
}

impl Vec3 {
    pub fn to_vec3a(&self) -> Vec3A {
        Vec3A {
            x: self.x,
            y: self.y,
            z: self.z,
        }
    }
}

#[spirv(fragment)]
pub fn main(input: Data<Vec3>, output: &mut Data<Vec3A>) {
    *output = Data {
        t: input.t.to_vec3a(),
        value: input.value,
    };
}
