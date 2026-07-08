// build-pass

use glam::UVec3;
use spirv_std::spirv;

#[repr(u32)]
pub enum Outcome {
    Fizz,
    Buzz,
    FizzBuzz,
}

trait Game {
    fn fizzbuzz(&self) -> Option<Outcome>;
}

impl Game for usize {
    fn fizzbuzz(&self) -> Option<Outcome> {
        match (self % 3 == 0, self % 5 == 0) {
            (true, true) => Some(Outcome::FizzBuzz),
            (true, false) => Some(Outcome::Fizz),
            (false, true) => Some(Outcome::Buzz),
            _ => None,
        }
    }
}

#[spirv(compute(threads(64)))]
pub fn main(
    #[spirv(global_invocation_id)] id: UVec3,
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] output: &mut [Option<Outcome>; 64],
) {
    let index = id.x as usize;
    output[index] = index.fizzbuzz();
}
