// Test case for issue #284: Fragment shader should NOT be silently compiled out
// This version should work correctly - uses assignment that creates dependency
// build-pass

use glam::{Vec3, Vec4, vec4};
use spirv_std::spirv;

#[spirv(fragment)]
pub fn main_fs(in_color: Vec3, out_frag_color: &mut Vec4) {
    // This works because it creates a dependency on the function
    *out_frag_color = vec4(in_color.x, in_color.y, in_color.z, 1.0);
}
