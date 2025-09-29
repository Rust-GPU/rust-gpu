//! Container for an untyped blob of data.

use core::mem;

#[spirv(buffer_load_intrinsic)]
// HACK(eddyb) try to prevent MIR inlining from breaking our intrinsics.
#[inline(never)]
#[spirv_std_macros::gpu_only]
unsafe fn buffer_load_intrinsic<T>(
    buffer: &[u32],
    // FIXME(eddyb) should be `usize`.
    offset: u32,
) -> T {
    unsafe {
        // NOTE(eddyb) this doesn't work with `rustc_codegen_spirv` and is only here
        // for explanatory purposes, and to cause some kind of verbose error if
        // `#[spirv(buffer_load_intrinsic)]` fails to replace calls to this function.
        buffer
            .as_ptr()
            .cast::<u8>()
            .add(offset as usize)
            .cast::<T>()
            .read()
    }
}

#[spirv(buffer_store_intrinsic)]
// HACK(eddyb) try to prevent MIR inlining from breaking our intrinsics.
#[inline(never)]
#[spirv_std_macros::gpu_only]
unsafe fn buffer_store_intrinsic<T>(
    buffer: &mut [u32],
    // FIXME(eddyb) should be `usize`.
    offset: u32,
    value: T,
) {
    unsafe {
        // NOTE(eddyb) this doesn't work with `rustc_codegen_spirv` and is only here
        // for explanatory purposes, and to cause some kind of verbose error if
        // `#[spirv(buffer_store_intrinsic)]` fails to replace calls to this function.
        buffer
            .as_mut_ptr()
            .cast::<u8>()
            .add(offset as usize)
            .cast::<T>()
            .write(value);
    }
}

/// `ByteAddressableBuffer` is a view to an untyped blob of data, allowing
/// loads and stores of arbitrary basic data types at arbitrary indices.
///
/// # Alignment
/// All data must be aligned to size 4, each element within the data (e.g.
/// struct fields) must have a size and alignment of a multiple of 4, and the
/// `byte_index` passed to load and store must be a multiple of 4. Technically
/// it is not a *byte* addressable buffer, but rather a *word* buffer, but this
/// naming and behavior was inherited from HLSL (where it's UB to pass in an
/// index not a multiple of 4).
///
/// # Safety
/// Using these functions allows reading a different type from the buffer than
/// was originally written (by a previous `store()` or the host API), allowing
/// all sorts of safety guarantees to be bypassed, making it effectively a
/// transmute.
#[repr(transparent)]
pub struct ByteAddressableBuffer<T> {
    /// The underlying array of bytes, able to be directly accessed.
    pub data: T,
}

fn bounds_check<T>(data: &[u32], byte_index: u32) {
    let sizeof = mem::size_of::<T>() as u32;
    if !byte_index.is_multiple_of(4) {
        panic!("`byte_index` should be a multiple of 4");
    }
    let last_byte = byte_index + sizeof;
    let len = data.len() as u32 * 4;
    if byte_index + sizeof > len {
        panic!(
            "index out of bounds: the len is {} but loading {} bytes at `byte_index` {} reads until {} (exclusive)",
            len, sizeof, byte_index, last_byte,
        );
    }
}

impl<'a> ByteAddressableBuffer<&'a [u32]> {
    /// Creates a `ByteAddressableBuffer` from the untyped blob of data.
    #[inline]
    pub fn from_slice(data: &'a [u32]) -> Self {
        Self { data }
    }

    /// Loads an arbitrary type from the buffer. `byte_index` must be a
    /// multiple of 4.
    ///
    /// # Safety
    /// See [`Self`].
    pub unsafe fn load<T>(&self, byte_index: u32) -> T {
        bounds_check::<T>(self.data, byte_index);
        unsafe { buffer_load_intrinsic(self.data, byte_index) }
    }

    /// Loads an arbitrary type from the buffer. `byte_index` must be a
    /// multiple of 4.
    ///
    /// # Safety
    /// See [`Self`]. Additionally, bounds or alignment checking is not performed.
    pub unsafe fn load_unchecked<T>(&self, byte_index: u32) -> T {
        unsafe { buffer_load_intrinsic(self.data, byte_index) }
    }
}

impl<'a> ByteAddressableBuffer<&'a mut [u32]> {
    /// Creates a `ByteAddressableBuffer` from the untyped blob of data.
    #[inline]
    pub fn from_mut_slice(data: &'a mut [u32]) -> Self {
        Self { data }
    }

    /// Create a non-mutable `ByteAddressableBuffer` from this mutable one.
    #[inline]
    pub fn as_ref(&self) -> ByteAddressableBuffer<&[u32]> {
        ByteAddressableBuffer { data: self.data }
    }

    /// Loads an arbitrary type from the buffer. `byte_index` must be a
    /// multiple of 4.
    ///
    /// # Safety
    /// See [`Self`].
    #[inline]
    pub unsafe fn load<T>(&self, byte_index: u32) -> T {
        unsafe { self.as_ref().load(byte_index) }
    }

    /// Loads an arbitrary type from the buffer. `byte_index` must be a
    /// multiple of 4.
    ///
    /// # Safety
    /// See [`Self`]. Additionally, bounds or alignment checking is not performed.
    #[inline]
    pub unsafe fn load_unchecked<T>(&self, byte_index: u32) -> T {
        unsafe { self.as_ref().load_unchecked(byte_index) }
    }

    /// Stores an arbitrary type into the buffer. `byte_index` must be a
    /// multiple of 4.
    ///
    /// # Safety
    /// See [`Self`].
    pub unsafe fn store<T>(&mut self, byte_index: u32, value: T) {
        bounds_check::<T>(self.data, byte_index);
        unsafe {
            buffer_store_intrinsic(self.data, byte_index, value);
        }
    }

    /// Stores an arbitrary type into the buffer. `byte_index` must be a
    /// multiple of 4.
    ///
    /// # Safety
    /// See [`Self`]. Additionally, bounds or alignment checking is not performed.
    pub unsafe fn store_unchecked<T>(&mut self, byte_index: u32, value: T) {
        unsafe {
            buffer_store_intrinsic(self.data, byte_index, value);
        }
    }
}
