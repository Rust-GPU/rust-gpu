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
pub fn store(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] buf: &mut RuntimeArray<
        TypedBuffer<[u32]>,
    >,
    #[spirv(flat)] index_in: u32,
    #[spirv(flat)] val: BigStruct,
) {
    unsafe {
        let mut buf = ByteAddressableBuffer::from_mut_slice(buf.index_mut(index_in as usize));
        buf.store(5, val);
    }
}
