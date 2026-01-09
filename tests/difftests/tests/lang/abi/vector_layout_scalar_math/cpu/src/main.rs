use abi_vector_layout_cpu::glam_features::GlamFeatures;

fn main() -> anyhow::Result<()> {
    abi_vector_layout_cpu::cpu_driver::run(GlamFeatures::ScalarMath);
}
