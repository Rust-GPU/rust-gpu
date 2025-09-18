#[repr(u32)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum GlamFeatures {
    Default,
    Cuda,
    ScalarMath,
}

impl GlamFeatures {
    pub fn is(&self) -> bool {
        match self {
            // cuda: 16 (!)
            // cuda + scalar-math: 8
            // scalar-math: (native) 4
            GlamFeatures::Default => align_of::<glam::Mat2>() == 16 && !GlamFeatures::Cuda.is(),
            // default, scalar-math: (native) 4
            GlamFeatures::Cuda => align_of::<glam::Vec2>() == 8,
            // default, cuda: 16
            GlamFeatures::ScalarMath => align_of::<glam::Vec4>() == 4,
        }
    }

    pub fn assert(&self) {
        for feature in [Self::Default, Self::Cuda, Self::ScalarMath] {
            if *self == feature {
                assert!(feature.is(), "Expected feature {feature:?} to be enabled");
            } else {
                assert!(
                    !feature.is(),
                    "Feature {feature:?} mistakenly enabled! Too aggressive feature unification?"
                );
            }
        }
    }
}
