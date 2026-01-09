use difftest::config::Config;
use difftest::scaffold::compute::{
    BufferConfig, BufferUsage, RustComputeShader, WgpuComputeTestPushConstants,
};

#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
pub struct PushConstants {
    multiplier: f32,
    offset: f32,
    flags: u32,
    count: u32,
}

fn main() -> anyhow::Result<()> {
    let config = Config::new()?;

    let num_elements = 256;
    let buffer_size = num_elements * 4; // 4 bytes per f32

    // Create input data
    let input_data: Vec<f32> = (0..num_elements).map(|i| i as f32 * 0.1).collect();
    let input_bytes: Vec<u8> = input_data.iter().flat_map(|&x| x.to_ne_bytes()).collect();

    // Create push constants data
    let push_constants = PushConstants {
        multiplier: 2.5,
        offset: 1.0,
        flags: 0, // Linear transformation
        count: num_elements as u32,
    };

    WgpuComputeTestPushConstants::new(
        RustComputeShader::default(),
        [4, 1, 1], // 256 / 64 = 4 workgroups
        vec![
            BufferConfig {
                size: buffer_size as u64,
                usage: BufferUsage::StorageReadOnly,
                initial_data: Some(input_bytes),
            },
            BufferConfig {
                size: buffer_size as u64,
                usage: BufferUsage::Storage,
                initial_data: None,
            },
        ],
        std::mem::size_of::<PushConstants>() as u32,
        bytemuck::bytes_of(&push_constants).to_vec(),
    )?
    .run_test(&config)
}
