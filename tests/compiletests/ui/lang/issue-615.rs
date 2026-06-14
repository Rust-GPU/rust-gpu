#![crate_name = "issue_615"]

// Tests that fallible integer conversions through `TryFrom`/`Result` can be
// codegen'd. This regressed after the nightly-2026-05-22 toolchain upgrade with
// errors like "cannot cast between pointer types" and "cannot offset a pointer
// to an arbitrary element" originating from `core::convert::num`.
//
// The root cause: `core::num::TryFromIntError` is no longer a ZST (it carries a
// niche-bearing error-kind `enum`), so `Result<u32, TryFromIntError>` is laid
// out as `BackendRepr::Memory` with its `Ok`/`Err` payloads at distinct offsets.
// Such multi-variant `enum`s previously had their payloads omitted from the
// generated SPIR-V type, breaking payload field accesses.

// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: u32, out: &mut u32) {
    let v = i as usize;

    // Direct conversion via `TryFrom` + `Result`/`Option` adapters.
    let mut acc = u32::try_from(v).ok().unwrap_or(0);

    // The `Result<u32, TryFromIntError>` also shows up indirectly inside
    // `StepBy`'s iteration, which is the form originally reported in #615.
    for index in (0..v).step_by(2) {
        acc = acc.wrapping_add(index as u32);
    }

    *out = acc;
}
