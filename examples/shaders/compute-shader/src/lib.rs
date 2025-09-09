#![cfg_attr(target_arch = "spirv", no_std)]
// HACK(eddyb) can't easily see warnings otherwise from `spirv-builder` builds.
#![deny(warnings)]

use glam::UVec3;
use spirv_std::{glam, spirv};

// Adapted from the wgpu hello-compute example

/// Returns the length of the Collatz sequence (excluding the starting number) for `n`. Returns
/// `None` if (a) `n` is zero, or (b) a number in the sequence overflows a `u32`.
///
/// # Examples
///
/// The sequence for 3 (excluding the starting number) is `[10, 5, 16, 8, 4, 2, 1]`, which has
/// length 7.
/// ```
/// # use compute_shader::collatz;
/// assert_eq!(collatz(3), Some(7));
/// ```
pub fn collatz(mut n: u32) -> Option<u32> {
    let mut i = 0;
    if n == 0 {
        return None;
    }
    while n != 1 {
        n = if n.is_multiple_of(2) {
            n / 2
        } else {
            // Overflow? (i.e. 3*n + 1 > 0xffff_ffff)
            if n >= 0x5555_5555 {
                return None;
            }
            // TODO: Use this instead when/if checked add/mul can work:
            // n.checked_mul(3)?.checked_add(1)?
            3 * n + 1
        };
        i += 1;
    }
    Some(i)
}

// LocalSize/numthreads of (x = 64, y = 1, z = 1)
#[spirv(compute(threads(64)))]
pub fn main_cs(
    #[spirv(global_invocation_id)] id: UVec3,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] prime_indices: &mut [u32],
) {
    let index = id.x as usize
        + core::mem::size_of_val(
            const {
                struct S<T: ?Sized>(T);
                &S([]) as &S<[()]>
            },
        );
    prime_indices[index] = collatz(prime_indices[index]).unwrap_or(u32::MAX);
}
