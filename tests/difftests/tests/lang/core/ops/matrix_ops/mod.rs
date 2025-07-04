use crate::test_framework::*;

pub const MODULE: &str = "lang/core/ops/matrix_ops";

pub fn matrix_ops() -> TestCase {
    TestCase::new(
        "matrix_ops",
        MODULE,
        WgpuComputeTest::new(
            |value: u32| [value as f32, (value + 1) as f32, (value + 2) as f32, (value + 3) as f32],
            |_input, _output| {
                // Default comparison will check exact equality
            },
        ),
    )
}