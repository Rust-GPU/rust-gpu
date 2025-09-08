#![no_std]

use spirv_std::spirv;

#[inline(never)]
fn sum_pair_u32(p: (u32, u32)) -> u32 {
    p.0.wrapping_add(p.1)
}

#[inline(never)]
fn mix_pair_u32_f32(p: (u32, f32)) -> u32 {
    p.0 ^ p.1.to_bits()
}

#[spirv(compute(threads(1)))]
pub fn main_cs(
    #[spirv(storage_buffer, descriptor_set = 0, binding = 0)] out: &mut [u32],
    #[spirv(push_constant)] pc: &(u32, u32),
) {
    let a: (u32, u32) = (10, 20);
    let b: (u32, f32) = (7, 3.5);
    let c: (f32, u32) = (2.25, 9);
    let d: (u32, u32) = (0xDEADBEEF, 0x12345678);
    let e: (i32, i32) = (-5, 12);

    let sum_a = a.0 + a.1;
    let mix_b = b.0 ^ b.1.to_bits();
    let mix_c = c.1 ^ c.0.to_bits();
    let d_and = d.0 & d.1;
    let d_or = d.0 | d.1;
    let d_xor = d.0 ^ d.1;
    let d_sum = d.0.wrapping_add(d.1);
    let e_sum_u32 = e.0.wrapping_add(e.1) as u32;

    #[repr(transparent)]
    struct Wrap((u32, u32));
    let w = Wrap((3, 4));
    let w_sum = (w.0).0 + (w.0).1;

    let reorder_sum = (1u32, 2u32).1 + (1u32, 2u32).0;

    out[0] = a.0;
    out[1] = a.1;
    out[2] = sum_a;
    out[3] = mix_b;
    out[4] = mix_c;
    out[5] = d_and;
    out[6] = d_or;
    out[7] = d_xor;
    out[8] = d_sum;
    out[9] = e_sum_u32;
    out[10] = w_sum;
    out[11] = reorder_sum;

    let p: (i32, u32) = (-123, 456);
    let p_mix = (p.0 as u32).wrapping_add(p.1);
    let q: (f32, f32) = (0.75, -1.5);
    let q_mix = q.0.to_bits() ^ q.1.to_bits();
    let r: ((u32, f32), (u32, f32)) = ((8, 0.5), (16, -2.0));
    let r_mix = (r.0).0 ^ (r.1).0 ^ (r.0).1.to_bits() ^ (r.1).1.to_bits();
    out[12] = p_mix;
    out[13] = q_mix;
    out[14] = r_mix;

    let tt_a: ((u32, u32), (u32, u32)) = ((1, 2), (3, 4));
    let tt_a_sum = tt_a.0.0 + tt_a.0.1 + tt_a.1.0 + tt_a.1.1;
    let tt_b: ((u32, f32), (f32, u32)) = ((5, 1.5), (2.25, 9));
    let tt_b_mix = tt_b.0.0 ^ tt_b.0.1.to_bits() ^ tt_b.1.0.to_bits() ^ tt_b.1.1;
    out[15] = tt_a_sum;
    out[16] = tt_b_mix;

    let call_sum = sum_pair_u32((11, 22));
    let call_mix = mix_pair_u32_f32((13, -0.75));
    out[17] = call_sum;
    out[18] = call_mix;

    let pc_sum = pc.0.wrapping_add(pc.1);
    let pc_xor = pc.0 ^ pc.1;
    out[19] = pc_sum;
    out[20] = pc_xor;
}
