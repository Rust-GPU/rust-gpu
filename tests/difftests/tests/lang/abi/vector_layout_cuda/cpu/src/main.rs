use abi_vector_layout_cpu::glam_features::GlamFeatures;

fn main() {
    abi_vector_layout_cpu::cpu_driver::run(GlamFeatures::Cuda);
}
