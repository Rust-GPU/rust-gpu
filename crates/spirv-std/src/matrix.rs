//! a set of common SPIR-V Matrices, used for intrinsics

use core::fmt::{Debug, Display, Formatter};
use glam::{Affine3A, Mat3, Mat3A, Mat4, Vec3, Vec3A};

/// A Matrix with 4 columns of [`Vec3`], very similar to glam's [`Affine3A`].
///
/// Primarily used in ray tracing extensions to represent object rotation, scale and translation.
///
/// # Limitations
/// These Limitations apply to all structs marked with `#[spirv(matrix)]`, which `Matrix4x3` is the only one in
/// `spirv-std`:
/// * Cannot be used within buffers, push constants or anything that requires an "explicit layout". Use [`Affine3A`],
/// [`Mat4`] or the combination of [`Mat3`] with [`Vec3`] instead and convert them to `Matrix4x3` in the shader.
/// * There may be other situations where this type may surprisingly fail!
#[derive(Clone, Copy, Default, PartialEq)]
#[repr(C)]
#[spirv(matrix)]
#[allow(missing_docs)]
pub struct Matrix4x3 {
    pub x_axis: Vec3A,
    pub y_axis: Vec3A,
    pub z_axis: Vec3A,
    pub w_axis: Vec3A,
}

/// The `from_*` fn signatures should match [`Affine3A`], to make it easier to switch to [`Affine3A`] later.
/// The `to_*` fn signatures are custom.
impl Matrix4x3 {
    /// Convert from glam's [`Affine3A`]
    pub fn from_affine3a(affine: Affine3A) -> Self {
        Self {
            x_axis: affine.x_axis,
            y_axis: affine.y_axis,
            z_axis: affine.z_axis,
            w_axis: affine.w_axis,
        }
    }

    /// Creates an affine transform from a 3x3 matrix (expressing scale, shear and
    /// rotation)
    pub fn from_mat3(mat3: Mat3) -> Self {
        Self::from_affine3a(Affine3A::from_mat3(mat3))
    }

    /// Creates an affine transform from a 3x3 matrix (expressing scale, shear and rotation)
    /// and a translation vector.
    ///
    /// Equivalent to `Affine3A::from_translation(translation) * Affine3A::from_mat3(mat3)`
    pub fn from_mat3_translation(mat3: Mat3, translation: Vec3) -> Self {
        Self::from_affine3a(Affine3A::from_mat3_translation(mat3, translation))
    }

    /// The given `Mat4` must be an affine transform,
    /// i.e. contain no perspective transform.
    pub fn from_mat4(m: Mat4) -> Self {
        Self::from_affine3a(Affine3A::from_mat4(m))
    }

    /// Convert to glam's [`Affine3A`]
    pub fn to_affine3a(self) -> Affine3A {
        Affine3A {
            matrix3: Mat3A {
                x_axis: self.x_axis,
                y_axis: self.y_axis,
                z_axis: self.z_axis,
            },
            translation: self.w_axis,
        }
    }

    /// Creates a 3x3 matrix representing the rotation and scale, cutting off the translation
    pub fn to_mat3a(self) -> Mat3A {
        self.to_affine3a().matrix3
    }

    /// Creates a 3x3 matrix representing the rotation and scale, cutting off the translation
    pub fn to_mat3(self) -> Mat3 {
        Mat3::from(self.to_mat3a())
    }

    /// Creates a 4x4 matrix from this affine transform
    pub fn to_mat4(self) -> Mat4 {
        Mat4::from(self.to_affine3a())
    }
}

impl Debug for Matrix4x3 {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(&self.to_mat4(), f)
    }
}

impl Display for Matrix4x3 {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        Display::fmt(&self.to_mat4(), f)
    }
}
