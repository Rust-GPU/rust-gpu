#![no_std]

use spirv_std::ptr::PhysicalPtr;
use spirv_std::spirv;

/// The only bound buffer: everything else is reached through these pointers.
#[repr(C)]
pub struct Roots {
    pub data: PhysicalPtr<u32>,
    pub list: PhysicalPtr<Node>,
    pub output: PhysicalPtr<Output>,
    pub data_len: u32,
    pub tail_len: u32,
}

#[repr(C)]
pub struct Node {
    pub next: PhysicalPtr<Node>,
    pub value: u32,
    pub _pad: u32,
}

#[repr(C)]
pub struct Output {
    pub sum: u32,
    pub hash: u32,
    pub len: u32,
    pub head_value: u32,
}

/// Bounds the list walk, so that a bad `next` can't hang the GPU.
const MAX_LEN: u32 = 16;

#[spirv(compute(threads(1)))]
pub fn main_cs(#[spirv(storage_buffer, descriptor_set = 0, binding = 0)] roots: &Roots) {
    unsafe {
        // Pointer offsetting and scalar loads.
        let mut sum = 0u32;
        let mut i = 0;
        while i < roots.data_len {
            sum = sum.wrapping_add(*roots.data.add(i as u64).as_ref_unchecked());
            i += 1;
        }

        // Struct field loads, including a pointer to the next node.
        let (mut hash, mut len) = (0u32, 0u32);
        let mut node = roots.list;
        while !node.is_null() && len < MAX_LEN {
            let n = node.as_ref_unchecked();
            hash = hash.wrapping_mul(31).wrapping_add(n.value);
            len += 1;
            node = n.next;
        }

        // Struct field stores, and a load through a reinterpreted pointer
        // (`Node::value` is at byte offset 8).
        let output = roots.output.as_mut_unchecked();
        output.sum = sum;
        output.hash = hash;
        output.len = len;
        output.head_value = *roots.list.cast::<u32>().byte_add(8).as_ref_unchecked();

        // Scalar stores past the 4 words of `Output`: the end of `data`, reversed.
        let words = roots.output.cast::<u32>();
        let mut k = 0;
        while k < roots.tail_len {
            let x = *roots
                .data
                .add((roots.data_len - 1 - k) as u64)
                .as_ref_unchecked();
            *words.add(4 + k as u64).as_mut_unchecked() = x;
            k += 1;
        }
    }
}
