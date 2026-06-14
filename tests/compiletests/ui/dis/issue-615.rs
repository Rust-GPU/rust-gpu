#![crate_name = "issue_615"]

// Tests the generated SPIR-V type shapes for multi-variant `enum`s whose layout
// is `BackendRepr::Memory` (i.e. not niche- or `ScalarPair`-optimized). Their
// variant payloads must appear as fields of the `OpTypeStruct`, otherwise field
// accesses into a payload can't be codegen'd into a valid `OpAccessChain` (which
// is what regressed in #615, where `Result<u32, TryFromIntError>` became such an
// `enum` once `TryFromIntError` stopped being a ZST).
//
// Here the payloads sit at distinct offsets, so every payload is exposed:
// `Multi` becomes `{ discriminant, u32, u32 }` and `Tri` becomes
// `{ discriminant, u32, u32, u32 }`.

// build-pass
// compile-flags: -C llvm-args=--disassemble-globals
// normalize-stderr-test "([A-Za-z]:)?/\S*/library/core/src/" -> "$$CORE_SRC/"
// normalize-stderr-test "OpCapability VulkanMemoryModel\n" -> ""
// normalize-stderr-test "OpSource .*\n" -> ""
// normalize-stderr-test "OpExtension .SPV_KHR_vulkan_memory_model.\n" -> ""
// normalize-stderr-test "OpMemoryModel Logical Vulkan" -> "OpMemoryModel Logical Simple"

// `compiletest` handles `ui\dis\`, but not `ui\\dis\\`, on Windows.
// normalize-stderr-test "ui/dis/" -> "$$DIR/"

use spirv_std::spirv;

// `B`'s two payload fields force `BackendRepr::Memory` (no single common
// primitive across variants) and sit at distinct offsets, so both are exposed
// alongside the discriminant.
pub enum Multi {
    A(u32),
    B(u32, u32),
}

pub enum Tri {
    A(u32),
    B(u32, u32),
    C(u32, u32, u32),
}

#[inline(never)]
fn read_multi(e: &Multi) -> u32 {
    match e {
        Multi::A(x) => *x,
        Multi::B(_, b) => *b,
    }
}

#[inline(never)]
fn read_tri(e: &Tri) -> u32 {
    match e {
        Tri::A(x) => *x,
        Tri::B(_, b) => *b,
        Tri::C(_, _, c) => *c,
    }
}

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: u32, out: &mut u32) {
    let m = if i > 0 { Multi::A(i) } else { Multi::B(1, 2) };
    let t = if i > 1 {
        Tri::A(i)
    } else if i == 1 {
        Tri::B(1, 2)
    } else {
        Tri::C(1, 2, 3)
    };
    *out = read_multi(&m).wrapping_add(read_tri(&t));
}
