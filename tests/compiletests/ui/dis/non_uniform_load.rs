// build-pass
// compile-flags: -Ctarget-feature=+ShaderNonUniform,+ext:SPV_EXT_descriptor_indexing
// compile-flags: -C llvm-args=--disassemble
// normalize-stderr-test "OpSource .*\n" -> ""

use spirv_std::{ByteAddressableBuffer, RuntimeArray, TypedBuffer, spirv};

pub struct BigStruct {
    a: u32,
    b: u32,
    c: u32,
    d: u32,
    e: u32,
    f: u32,
}

#[spirv(fragment)]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &RuntimeArray<
        TypedBuffer<[u32]>,
    >,
    #[spirv(flat)] index_in: u32,
    out: &mut BigStruct,
) {
    unsafe {
        let buf = ByteAddressableBuffer::from_slice(buf.index(index_in as usize));
        *out = buf.load(5);
    }
}
