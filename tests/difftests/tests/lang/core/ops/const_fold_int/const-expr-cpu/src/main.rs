use const_fold_int_const_fold_cpu::Variants;

fn main() -> anyhow::Result<()> {
    const_fold_int_const_fold_cpu::cpu_driver::run(Variants::ConstExpr);
}
