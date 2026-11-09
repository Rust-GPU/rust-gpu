// Tests using a vector like pointer at `const` time.
// build-pass

use spirv_std::spirv;

use core::ptr::NonNull;

const VEC_LIKE: (NonNull<usize>, usize, usize) = (NonNull::<usize>::dangling(), 0, 0);

pub fn assign_vec_like() {
    let _vec_like = VEC_LIKE;
}

#[spirv(fragment)]
pub fn main() {}
