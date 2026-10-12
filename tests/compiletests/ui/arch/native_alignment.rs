// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main() {
    const _: () = {
        assert!(align_of::<i8>() == 1);
        assert!(align_of::<i16>() == 2);
        assert!(align_of::<i32>() == 4);
        assert!(align_of::<i64>() == 8);
        assert!(align_of::<i128>() == 16);
        assert!(align_of::<isize>() == 4);
        assert!(align_of::<u8>() == 1);
        assert!(align_of::<u16>() == 2);
        assert!(align_of::<u32>() == 4);
        assert!(align_of::<u64>() == 8);
        assert!(align_of::<u128>() == 16);
        assert!(align_of::<usize>() == 4);
    };
}
