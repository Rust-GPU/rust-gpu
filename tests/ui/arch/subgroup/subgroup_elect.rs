// build-pass
// compile-flags: -C target-feature=+GroupNonUniform,+ext:SPV_KHR_vulkan_memory_model
// compile-flags: -C llvm-args=--disassemble-fn=subgroup_elect::subgroup_elect

use spirv_std::spirv;

unsafe fn subgroup_elect() -> bool {
    spirv_std::arch::subgroup_elect()
}

#[spirv(compute(threads(1, 1, 1)))]
pub fn main() {
    unsafe {
        subgroup_elect();
    }
}
