use difftest::config::{Config, TestMetadata};
use std::{fs, io::Write};

fn main() {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();

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

    let w_sum = 3u32 + 4u32;
    let reorder_sum = 1u32 + 2u32;

    let mut out = [0u32; 21];
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
    let r_mix = 8u32 ^ 16u32 ^ 0.5f32.to_bits() ^ (-2.0f32).to_bits();
    out[12] = p_mix;
    out[13] = q_mix;
    out[14] = r_mix;

    let tt_a_sum = 1u32 + 2 + 3 + 4;
    let tt_b_mix = 5u32 ^ 1.5f32.to_bits() ^ 2.25f32.to_bits() ^ 9u32;
    out[15] = tt_a_sum;
    out[16] = tt_b_mix;

    let call_sum = 11u32.wrapping_add(22u32);
    let call_mix = 13u32 ^ (-0.75f32).to_bits();
    out[17] = call_sum;
    out[18] = call_mix;

    // Push constants equivalent values (must match GPU push data)
    let pc0: u32 = 100;
    let pc1: u32 = 200;
    let pc_sum = pc0.wrapping_add(pc1);
    let pc_xor = pc0 ^ pc1;
    out[19] = pc_sum;
    out[20] = pc_xor;

    let mut file = fs::File::create(&config.output_path).expect("Failed to create output file");
    file.write_all(bytemuck::cast_slice(&out))
        .expect("Failed to write output");
    config
        .write_metadata(&TestMetadata::u32())
        .expect("Failed to write metadata");
}
