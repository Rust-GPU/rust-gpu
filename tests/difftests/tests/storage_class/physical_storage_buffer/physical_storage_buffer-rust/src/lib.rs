#![no_std]

use spirv_std::spirv;

#[repr(C)]
pub struct Roots {
    pub head: u32,
    pub data_len: u32,
    pub tail_len: u32,
}

#[repr(C)]
pub struct Node {
    pub next: u32,
    pub value: u32,
}

const NULL: u32 = u32::MAX;
const MAX_LEN: u32 = 16;

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] roots: &Roots,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 1)] data: &[u32],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 2)] nodes: &[Node],
    #[spirv(storage_buffer, descriptor_set = 0, binding = 3)] output: &mut [u32; 8],
) {
    let mut sum = 0u32;
    let mut i = 0;
    while i < roots.data_len {
        sum = sum.wrapping_add(data[i as usize]);
        i += 1;
    }

    let (mut hash, mut len) = (0u32, 0u32);
    let mut node = roots.head;
    while node != NULL && len < MAX_LEN {
        let n = &nodes[node as usize];
        hash = hash.wrapping_mul(31).wrapping_add(n.value);
        len += 1;
        node = n.next;
    }

    output[0] = sum;
    output[1] = hash;
    output[2] = len;
    output[3] = nodes[roots.head as usize].value;

    let mut k = 0;
    while k < roots.tail_len {
        output[4 + k as usize] = data[(roots.data_len - 1 - k) as usize];
        k += 1;
    }
}
