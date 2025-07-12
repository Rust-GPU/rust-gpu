use difftest::config::Config;
use std::fs::File;
use std::io::Write;

fn main() {
    let mut buf = [0u32; 256];
    blake3_rust_gpu::main_cs(&mut buf);
    write_cpu_test(bytemuck::bytes_of(&buf)).unwrap();
}

fn write_cpu_test(output: &[u8]) -> anyhow::Result<()> {
    let config = Config::from_path(std::env::args().nth(1).unwrap()).unwrap();
    let mut f = File::create(&config.output_path)?;
    f.write_all(&output)?;
    Ok(())
}
