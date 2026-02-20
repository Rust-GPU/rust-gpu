//! Defines the [`UniqueIndex`] and [`UniqueId`], describing a unique index or id for various [`Scope`]s

use crate::unique::scope::*;
use core::marker::PhantomData;
use core::ops::Deref;
use glam::UVec3;

/// A `UniqueId` is a 3D index, represented as [`UVec3`], that is unique within the [`ActiveInvocations`] scope.
///
/// # Safety
/// The index must be globally unique within the [`ActiveInvocations`] scope.
pub type ActiveInvocationsUniqueId = UniqueId<ActiveInvocations>;

/// A `UniqueId` is a 3D index, represented as [`UVec3`], that is unique within the [`Subgroup`] scope.
///
/// # Safety
/// The index must be globally unique within the [`Subgroup`] scope.
pub type SubgroupUniqueId = UniqueId<Subgroup>;

/// A `UniqueId` is a 3D index, represented as [`UVec3`], that is unique within the [`Workgroup`] scope.
///
/// # Safety
/// The index must be globally unique within the [`Workgroup`] scope.
pub type WorkgroupUniqueId = UniqueId<Workgroup>;

/// A `UniqueId` is a 3D index, represented as [`UVec3`], that is unique within the [`Global`] scope.
///
/// # Safety
/// The index must be globally unique within the [`Global`] scope.
pub type GlobalUniqueId = UniqueId<Global>;

/// A `UniqueId` is a 3D index, represented as [`UVec3`], that is unique within the generic [`Scope`] `S`.
///
/// # Safety
/// The index must be globally unique within the [`Scope`] `S`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct UniqueId<S: Scope> {
    index: UVec3,
    _phantom: PhantomData<S>,
}

impl<S: Scope> UniqueId<S> {
    /// Create a new [`UniqueId`]
    ///
    /// # Safety
    /// Index must be unique within the [`Scope`] `S`.
    #[inline]
    pub const unsafe fn new_unchecked(index: UVec3) -> Self {
        Self {
            index,
            _phantom: PhantomData,
        }
    }
}

impl<S: Scope> Deref for UniqueId<S> {
    type Target = UVec3;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.index
    }
}

impl<S: Scope> UniqueId<S> {
    /// Convert to `UVec3`
    pub const fn to_uvec3(&self) -> UVec3 {
        self.index
    }
}

impl<S: Scope> From<UniqueId<S>> for UVec3 {
    fn from(inner: UniqueId<S>) -> Self {
        inner.index
    }
}

macro_rules! impl_downcast {
    ($name:ident, $from:ident, $to:ident) => {
        impl<S: $from> UniqueId<S> {
            #[doc = concat!("Downcast this [`UniqueId`] to the [`", stringify!($to), "`] scope")]
            pub fn $name(&self) -> UniqueId<$to> {
                // Safety: downcasting scopes to one with fewer guarantees is safe
                unsafe { UniqueId::new_unchecked(self.index) }
            }
        }
    };
}

impl_downcast!(
    to_active_invocations_scope,
    AtLeastActiveInvocations,
    ActiveInvocations
);
impl_downcast!(to_subgroup_scope, AtLeastSubgroup, Subgroup);
impl_downcast!(to_workgroup_scope, AtLeastWorkgroup, Workgroup);
impl_downcast!(to_global_scope, AtLeastGlobal, Global);
