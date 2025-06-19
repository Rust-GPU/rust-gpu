// Test case for issue #284: Fragment shader silently compiled out
// This version should now fail with an error instead of silently compiling out

use glam::{Vec3, Vec4};
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main_fs(in_color: Vec3, out_frag_color: &mut Vec4) {
    // This pattern gets optimized away and should trigger an error
    // because it doesn't properly initialize the output
    out_frag_color.x = in_color.x;
    out_frag_color.y = in_color.y;
    out_frag_color.z = in_color.z;
    // Missing: out_frag_color.w = 1.0;
}
