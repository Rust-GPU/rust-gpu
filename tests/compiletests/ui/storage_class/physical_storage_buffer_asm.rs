// build-pass
// compile-flags: -C target-feature=+PhysicalStorageBufferAddresses,+Int64,+ext:SPV_KHR_physical_storage_buffer
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "\n\W*OpCapability VulkanMemoryModel" -> ""
// normalize-stderr-test "\n\W*OpSource .*" -> ""
// normalize-stderr-test "\n\W*%\d+ = OpString .*" -> ""
// normalize-stderr-test "\n\W*OpExtension .SPV_KHR_vulkan_memory_model." -> ""
// normalize-stderr-test "OpMemoryModel PhysicalStorageBuffer64 Vulkan" -> "OpMemoryModel PhysicalStorageBuffer64 Simple"

// Before SPIR-V 1.4, `OpEntryPoint` only lists `Input`/`Output` variables,
// which also changes the order of globals in the disassembly.
// ignore-spv1.0
// ignore-spv1.1
// ignore-spv1.2
// ignore-spv1.3
// ignore-vulkan1.0
// ignore-vulkan1.1

use core::arch::asm;
use spirv_std::spirv;

#[spirv(compute(threads(1)))]
pub fn main(
    #[spirv(push_constant)] addr: &u64,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] out: &mut u32,
) {
    unsafe {
        asm!(
            "%ptr = OpConvertUToPtr typeof{out} {addr}",
            "%val = OpLoad _ %ptr Aligned 4",
            "OpStore {out} %val",
            addr = in(reg) *addr,
            out = in(reg) out,
        );
    }
}
