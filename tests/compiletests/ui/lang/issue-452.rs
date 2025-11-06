// build-fail

use spirv_std::spirv;

#[derive(Clone, Copy)]
struct Position(u32);

fn use_cmp(cmp: fn(&Position) -> u32) {
    let a = Position(0);
    let b = Position(1);

    let _ = if cmp(&a) <= cmp(&b) { a } else { b };
}

#[spirv(compute(threads(1)))]
pub fn main() {
    use_cmp(|p| p.0);
}
