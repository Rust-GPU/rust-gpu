use crate::unique::{Global, GlobalUniqueId, GlobalUniqueIndex, ScalarValue};
use core::arch::asm;
use glam::UVec3;

#[inline]
pub fn global_invocation_id() -> GlobalUniqueId {
    unsafe {
        let slot = UVec3::default();
        asm! {
            "%builtin = OpVariable typeof{result} Input",
            "OpDecorate %builtin BuiltIn GlobalInvocationId",
            "%result = OpLoad typeof*{result} %builtin",
            "OpStore {result} %result",
            result = in(reg) &slot,
        }
        GlobalUniqueId::new_unchecked(slot)
    }
}

// TODO: this built-in doesn't work and is deprecated
#[inline]
pub fn workgroup_size() -> ScalarValue<UVec3, Global> {
    unsafe {
        let slot = UVec3::default();
        asm! {
            "%builtin = OpVariable typeof{result} Input",
            "OpDecorate %builtin BuiltIn WorkgroupSize",
            "%result = OpLoad typeof*{result} %builtin",
            "OpStore {result} %result",
            result = in(reg) &slot,
        }
        ScalarValue::new(slot)
    }
}

#[inline]
pub fn num_workgroups() -> ScalarValue<UVec3, Global> {
    unsafe {
        let slot = UVec3::default();
        asm! {
            "%builtin = OpVariable typeof{result} Input",
            "OpDecorate %builtin BuiltIn NumWorkgroups",
            "%result = OpLoad typeof*{result} %builtin",
            "OpStore {result} %result",
            result = in(reg) &slot,
        }
        ScalarValue::new(slot)
    }
}

#[inline]
pub fn global_invocation_index() -> GlobalUniqueIndex {
    unsafe {
        let gid = global_invocation_id();
        let wg_cnt = workgroup_size();
        GlobalUniqueIndex::new_unchecked(gid.x + gid.y * wg_cnt.x + gid.z * wg_cnt.y * wg_cnt.x)
    }
}
