// Test that accesses through `PhysicalStorageBuffer` pointers get the `Aligned`
// memory operand that SPIR-V requires (but which codegen doesn't emit).

// build-pass
// compile-flags: -C target-feature=+PhysicalStorageBufferAddresses,+Int64,+ext:SPV_KHR_physical_storage_buffer
// compile-flags: -C llvm-args=--disassemble-entry=main

use core::arch::asm;
use spirv_std::spirv;

#[repr(C)]
pub struct Addrs {
    x: u64,
    y: u64,
    src: u64,
    dst: u64,
}

// Not a scalar pair, so that it's copied as a whole.
#[derive(Copy, Clone)]
#[repr(C)]
pub struct S {
    a: u32,
    b: u32,
    c: u64,
}

unsafe fn physical<T>(addr: u64) -> *mut T {
    let result: *mut T;
    unsafe {
        asm!(
            "{result} = OpConvertUToPtr typeof{result} {addr}",
            addr = in(reg) addr,
            result = out(reg) result,
        );
    }
    result
}

#[spirv(compute(threads(1)))]
pub fn main(#[spirv(push_constant)] addrs: &Addrs) {
    unsafe {
        let x = physical::<u32>(addrs.x);
        let y = physical::<u64>(addrs.y);
        let src = physical::<S>(addrs.src);
        let dst = physical::<S>(addrs.dst);

        // An explicit `Aligned` is kept as-is.
        let x_aligned_16: u32;
        asm!(
            "{v} = OpLoad typeof{v} {x} Aligned 16",
            x = in(reg) x,
            v = out(reg) x_aligned_16,
        );
        *x ^= x_aligned_16 ^ *y as u32 ^ (&*src).a;

        *dst = *src;

        // `*dst = *src` copies via a local variable (later removed by `mem2reg`),
        // so `OpCopyMemory` between physical pointers has to be written by hand.
        asm!("OpCopyMemory {dst} {src}", dst = in(reg) dst, src = in(reg) src);
    }
}
