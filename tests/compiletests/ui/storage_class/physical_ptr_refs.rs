// Test references obtained from `spirv_std::ptr::PhysicalPtr` in common patterns.

// build-pass
// compile-flags: -C target-feature=+PhysicalStorageBufferAddresses,+Int64,+ext:SPV_KHR_physical_storage_buffer

use spirv_std::arch::atomic_i_add;
use spirv_std::memory::{Scope, Semantics};
use spirv_std::ptr::PhysicalPtr;
use spirv_std::spirv;

#[repr(C)]
pub struct Node {
    next: PhysicalPtr<Node>,
    value: u32,
}

#[repr(C)]
pub struct Stats {
    count: u32,
    sum: u32,
    last: u32,
}

#[repr(C)]
pub struct Buffers {
    list: PhysicalPtr<Node>,
    stats: PhysicalPtr<Stats>,
    triple: PhysicalPtr<[u32; 3]>,
    counter: PhysicalPtr<u32>,
}

fn sum3(xs: &[u32; 3]) -> u32 {
    xs[0] + xs[1] + xs[2]
}

#[spirv(compute(threads(1)))]
pub fn main(#[spirv(push_constant)] buffers: &Buffers) {
    unsafe {
        // A linked list, with each `next` read from the buffer it points into.
        let (mut count, mut sum, mut last) = (0, 0, 0);
        let mut node = buffers.list;
        while !node.is_null() {
            let n = node.as_ref_unchecked();
            count += 1;
            sum += n.value;
            last = n.value;
            node = n.next;
        }

        // Writes to the fields of a struct.
        let stats = buffers.stats.as_mut_unchecked();
        stats.count = count;
        stats.sum = sum;
        stats.last = last;

        // The same function called with a physical and a local reference.
        let local = [count, sum, last];
        let total = sum3(buffers.triple.as_ref_unchecked()) + sum3(&local);

        // An atomic on physical memory.
        atomic_i_add::<_, { Scope::Workgroup as u32 }, { Semantics::NONE.bits() }>(
            buffers.counter.as_mut_unchecked(),
            total,
        );
    }
}
