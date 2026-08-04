// build-pass

#![feature(adt_const_params)]
#![allow(incomplete_features)]

use spirv_std::memory::{Scope, Semantics};
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    unsafe {
        spirv_std::arch::memory_barrier::<
            { Scope::Subgroup },
            { Semantics::ACQUIRE_RELEASE.union(Semantics::UNIFORM_MEMORY) },
        >();
    }
}
