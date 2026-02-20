use crate::arch::IndexUnchecked;
use crate::unique::GlobalUniqueIndex;
use crate::unique::builtin::global_invocation_index;
use crate::{TypedBuffer, const_scalar};
use core::ops::{Index, IndexMut};

pub struct DisjointSlice<'a, T> {
    buffer: &'a mut TypedBuffer<[T]>,
}

impl<'a, T> DisjointSlice<'a, T> {
    pub fn new(buffer: &'a mut TypedBuffer<[T]>) -> Self {
        Self { buffer }
    }

    pub fn partition(&mut self) -> &mut T {
        &mut self.buffer[global_invocation_index()]
    }

    pub fn partition_many<const N: usize>(&mut self) -> &mut [T; N] {
        todo!("fails rust-gpu compilation")
        // let offset = (global_invocation_index() * const_scalar!(N as u32)).to_usize();
        // &mut self.buffer[offset..(offset+N)]
    }
}

impl<T> Index<GlobalUniqueIndex> for DisjointSlice<'_, T> {
    type Output = T;

    fn index(&self, index: GlobalUniqueIndex) -> &Self::Output {
        &self.buffer[index]
    }
}

impl<T> IndexMut<GlobalUniqueIndex> for DisjointSlice<'_, T> {
    fn index_mut(&mut self, index: GlobalUniqueIndex) -> &mut Self::Output {
        &mut self.buffer[index]
    }
}

impl<T> IndexUnchecked<T> for DisjointSlice<'_, T> {
    unsafe fn index_unchecked(&self, index: usize) -> &T {
        self.buffer.index_unchecked(index)
    }

    unsafe fn index_unchecked_mut(&mut self, index: usize) -> &mut T {
        self.buffer.index_unchecked_mut(index)
    }
}
