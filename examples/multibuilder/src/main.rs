use spirv_builder::SpirvBuilder;

fn main() {
    let mut builder = SpirvBuilder::new(
        concat!(env!("CARGO_MANIFEST_DIR"), "/../shaders/sky-shader"),
        "spirv-unknown-spv1.3",
    );
    builder.multimodule = true;
    let result = builder.build().unwrap();
    println!("{result:#?}");
}
