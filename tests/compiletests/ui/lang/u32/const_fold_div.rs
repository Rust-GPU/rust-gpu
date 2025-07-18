// build-pass

#![allow(unconditional_panic)]

use spirv_std::spirv;

#[spirv(fragment)]
pub fn const_fold_div(out: &mut u32) {
    *out = 7u32 / 0;
}
