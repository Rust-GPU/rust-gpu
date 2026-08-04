// build-pass

use spirv_std::spirv;

/// Regression test for `memset` on structs, which used to `fatal!("memset on structs not
/// implemented yet")`. `core::ptr::write_bytes` on a struct-typed pointer (as used internally
/// by e.g. `core::mem::zeroed::<T>()`) lowers to a single `memset` whose pointee type is the
/// struct itself, rather than one of its fields.
///
/// <https://github.com/Rust-GPU/rust-gpu/issues/328>
#[derive(Clone, Copy)]
pub struct Foo {
    a: u32,
    b: [u8; 7],
    c: u64,
}

/// Exercises `memset_const_pattern` for `SpirvType::Adt`, since both the fill byte and the size
/// are known at compile time.
pub fn zeroed_struct() -> Foo {
    unsafe { core::mem::zeroed() }
}

/// Exercises `memset_dynamic_pattern` for `SpirvType::Adt`, since the fill byte is not known at
/// compile time.
pub fn filled_struct(byte: u8) -> Foo {
    let mut foo = Foo {
        a: 0,
        b: [0; 7],
        c: 0,
    };
    unsafe {
        core::ptr::write_bytes(&mut foo, byte, 1);
    }
    foo
}

#[spirv(fragment)]
pub fn main() {}
