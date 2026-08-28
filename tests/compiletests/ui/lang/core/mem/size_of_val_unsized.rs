use spirv_std::spirv;

#[spirv(vertex)]
pub fn main(out: &mut usize) {
    *out = core::mem::size_of_val(
        const {
            struct S<T: ?Sized>(T);
            &S([]) as &S<[()]>
        },
    );
}
