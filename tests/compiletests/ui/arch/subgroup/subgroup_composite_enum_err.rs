// build-fail
// normalize-stderr-test "\S*/crates/spirv-std/src/" -> "$$SPIRV_STD_SRC/"
// normalize-stderr-test "\.rs:\d+:\d+" -> ".rs:"
// normalize-stderr-test "(\n)\d* *([ -])([\|\+\-\=])" -> "$1   $2$3"

use glam::*;
use spirv_std::ScalarOrVectorComposite;
use spirv_std::arch::*;
use spirv_std::spirv;

macro_rules! enum_repr_from {
    ($ident:ident, $repr:ty) => {
        impl From<$repr> for $ident {
            #[inline]
            fn from(value: $repr) -> Self {
                match value {
                    0 => Self::A,
                    1 => Self::B,
                    2 => Self::C,
                    _ => Self::default(),
                }
            }
        }

        impl From<$ident> for $repr {
            #[inline]
            fn from(value: $ident) -> Self {
                value as $repr
            }
        }
    };
}

#[derive(Copy, Clone, Default, ScalarOrVectorComposite)]
pub enum NoRepr {
    #[default]
    A,
    B,
    C,
}

#[repr(u32)]
#[repr(u16)]
#[derive(Copy, Clone, Default, ScalarOrVectorComposite)]
pub enum TwoRepr {
    #[default]
    A,
    B,
    C,
}

#[repr(C)]
#[derive(Copy, Clone, Default, ScalarOrVectorComposite)]
pub enum CRepr {
    #[default]
    A,
    B,
    C,
}

#[repr(i32)]
#[derive(Copy, Clone, Default, ScalarOrVectorComposite)]
pub enum NoFrom {
    #[default]
    A,
    B,
    C,
}

#[repr(i32)]
#[derive(Copy, Clone, Default, ScalarOrVectorComposite)]
pub enum WrongFrom {
    #[default]
    A,
    B,
    C,
}

enum_repr_from!(WrongFrom, u32);

#[repr(i32)]
#[derive(Copy, Clone, ScalarOrVectorComposite)]
pub enum NoDefault {
    A,
    B,
    C,
}

enum_repr_from!(NoDefault, i32);
