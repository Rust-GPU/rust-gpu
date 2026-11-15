// Test that filling the smaller field of a union is an error, instead of being ignored.
// build-fail

use spirv_std::macros::spirv;

pub union Words {
    two: [u32; 2],
    four: [u32; 4],
}

#[spirv(fragment)]
pub fn main(o: &mut u32) {
    let mut words = Words { four: [1; 4] };
    words.two = [0; 2];
    *o = unsafe { words.four[3] };
}
