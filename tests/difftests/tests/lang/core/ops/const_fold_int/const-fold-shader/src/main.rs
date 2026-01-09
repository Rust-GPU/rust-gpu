use const_fold_int_const_fold_cpu::Variants;

fn main() -> anyhow::Result<()> {
    const_fold_int_const_fold_cpu::shader_driver::run(Variants::ConstFold);
}
