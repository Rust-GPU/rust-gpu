//! support functions for debug printf

use crate::{Scalar, Vector};

#[doc(hidden)]
pub fn assert_is_type<T>(ty: T) -> T {
    ty
}

#[doc(hidden)]
pub fn assert_is_vector<TY: Scalar, V: Vector<TY, SIZE>, const SIZE: usize>(vec: V) -> V {
    vec
}
