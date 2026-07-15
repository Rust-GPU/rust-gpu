// Tests that small entry-points aren't internalized by rustc and dropped
// before reaching codegen. See https://github.com/Rust-GPU/rust-gpu/issues/590.

// build-pass
// compile-flags: -C debuginfo=0 -C llvm-args=--disassemble-globals
// normalize-stderr-test "\n\W*OpCapability VulkanMemoryModel" -> ""
// normalize-stderr-test "\n\W*OpSource .*" -> ""
// normalize-stderr-test "\n\W*OpExtension .SPV_KHR_vulkan_memory_model." -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

// HACK(eddyb) `compiletest` handles `ui\dis\`, but not `ui\\dis\\`, on Windows.
// normalize-stderr-test "ui/dis/" -> "$$DIR/"

use spirv_std::glam::{UVec3, Vec4};
use spirv_std::spirv;

#[spirv(vertex)]
pub fn main_vs(#[spirv(vertex_index)] _v: i32, #[spirv(position)] _pos: &mut Vec4) {}

#[spirv(fragment)]
pub fn main_fs(_out: &mut Vec4) {}

#[spirv(compute(threads(64)))]
pub fn main_cs(#[spirv(global_invocation_id)] _id: UVec3) {}
