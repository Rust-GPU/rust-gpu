//! Physical pointers

#[cfg(target_arch = "spirv")]
use core::arch::asm;
use core::marker::PhantomData;

/// A physical pointer in the `PhysicalStorageBuffer` storage class
/// with semantics similar to `*mut T`.
///
/// This is similar to a raw pointer retrieved through `u64 as *mut T`, but
/// provides utilities for pointer manipulation that are currently not
/// supported on raw pointers due to the otherwise logical addressing model
/// and 32-bit pointer size.
pub struct PhysicalPtr<T> {
    // Use uvec2 instead of u64 to avoid demepndency on the Int64 dependency.
    addr: glam::UVec2,
    //addr: u64,
    _marker: PhantomData<*mut T>,
}

impl<T> Copy for PhysicalPtr<T> {}

impl<T> Clone for PhysicalPtr<T> {
    fn clone(&self) -> Self {
        Self {
            addr: self.addr,
            _marker: PhantomData,
        }
    }
}

impl<T> PhysicalPtr<T> {
    /// Get a mutaple pointer to the physical address.
    /// The same aliasing rules that apply to FFI, apply to the returned pointer.
    #[crate::macros::gpu_only]
    pub fn get(self) -> *mut T {
        let result: *mut T;
        unsafe {
            // FIXME(jwollen) add a way to dereference the result type further
            // or to pass type parameters
            let dummy: T = core::mem::MaybeUninit::uninit().assume_init();
            asm!(
                "%ptr_type = OpTypePointer PhysicalStorageBuffer typeof*{dummy}",
                "{result} = OpBitcast %ptr_type {addr}",
                addr = in(reg) &self.addr,
                dummy = in(reg) &dummy,
                result = out(reg) result,
            );
            result
        }
    }

    /// Creates a null physical pointer.
    pub fn null() -> Self {
        Self {
            addr: glam::UVec2::ZERO,
            _marker: PhantomData,
        }
    }

    /// Returns `true` if the pointer is null.
    pub fn is_null(self) -> bool {
        self.addr == glam::UVec2::ZERO
    }

    /// Casts to a pointer of another type.
    pub fn cast<U>(self) -> PhysicalPtr<U> {
        PhysicalPtr { addr: self.addr, _marker: PhantomData }
    }

    /// Returns `None` if the pointer is null, or else returns a shared reference to the value wrapped in `Some`.
    pub unsafe fn as_ref<'a>(self) -> Option<&'a T> {
        self.is_null().then_some(unsafe { self.as_ref_unchecked() })
    }

    /// Returns `None` if the pointer is null, or else returns a mutable reference to the value wrapped in `Some`.
    pub unsafe fn as_mut<'a>(self) -> Option<&'a mut T> {
        self.is_null().then_some(unsafe { self.as_mut_unchecked() })
    }

    /// Returns a shared reference to the value behind the pointer.
    pub unsafe fn as_ref_unchecked<'a>(self) -> &'a T {
        unsafe { &*self.get() }
    }

    /// Returns a mutable reference to the value behind the pointer.
    pub unsafe fn as_mut_unchecked<'a>(self) -> &'a mut T {
        unsafe { &mut *self.get() }
    }

    /// Gets the address portion of the pointer. All physical pointers are considered to have global provenance.
    pub fn addr(self) -> u64 {
        unsafe { core::mem::transmute(self.addr) }
    }

    /// Forms a physical pointer from an address. All physical pointers are considered to have global provenance.
    pub fn from_addr(addr: u64) -> Self {
        Self {
            addr: unsafe { core::mem::transmute(addr) },
            _marker: PhantomData,
        }
    }

    /// Creates a new pointer by mapping `self`’s address to a new one.
    pub fn map_addr(self, f: impl FnOnce(u64) -> u64) -> Self {
        Self::from_addr(f(self.addr()))
    }

    /// Adds a signed offset to a pointer.
    pub unsafe fn offset(self, count: i64) -> Self {
        unsafe { self.byte_offset(count * core::mem::size_of::<T>() as i64) }
    }

    /// Adds a signed offset in bytes to a pointer.
    pub unsafe fn byte_offset(self, count: i64) -> Self {
        self.map_addr(|addr| addr.overflowing_add_signed(count).0)
    }
}
