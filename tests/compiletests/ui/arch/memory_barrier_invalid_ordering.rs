// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// normalize-stderr-test "crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// normalize-stderr-test "\S*/lib/rustlib/" -> "$$SYSROOT/lib/rustlib/"
// normalize-stderr-test "\$SYSROOT/lib/rustlib/src/rust/library/core/src/panic.rs:\d+:\d+" -> "$$SYSROOT/lib/rustlib/src/rust/library/core/src/panic.rs:LL:CC"

#![feature(adt_const_params)]
#![allow(incomplete_features)]

use spirv_std::memory::{Scope, Semantics};
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    unsafe {
        // ACQUIRE and RELEASE are both memory-ordering flags, and the SPIR-V spec allows at
        // most one to be set at the same time.
        spirv_std::arch::memory_barrier::<
            { Scope::Subgroup },
            { Semantics::ACQUIRE.union(Semantics::RELEASE) },
        >();
    }
}
