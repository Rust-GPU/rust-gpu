use crate::unique::{
    ActiveInvocations, AtLeastActiveInvocations, AtLeastGlobal, AtLeastSubgroup, AtLeastWorkgroup,
    Global, Scope, Subgroup, Workgroup,
};
use core::marker::PhantomData;
use core::ops::{Deref, Index, IndexMut};

/// A `UniqueIndex` is an index that is unique within the [`ActiveInvocations`] scope.
///
/// # Safety
/// The index must be globally unique within the [`ActiveInvocations`] scope.
pub type ActiveInvocationsUniqueIndex = UniqueIndex<ActiveInvocations>;

/// A `UniqueIndex` is an index that is unique within the [`Subgroup`] scope.
///
/// # Safety
/// The index must be globally unique within the [`Subgroup`] scope.
pub type SubgroupUniqueIndex = UniqueIndex<Subgroup>;

/// A `UniqueIndex` is an index that is unique within the [`Workgroup`] scope.
///
/// # Safety
/// The index must be globally unique within the [`Workgroup`] scope.
pub type WorkgroupUniqueIndex = UniqueIndex<Workgroup>;

/// A `UniqueIndex` is an index that is unique within the [`Global`] scope.
///
/// # Safety
/// The index must be globally unique within the [`Global`] scope.
pub type GlobalUniqueIndex = UniqueIndex<Global>;

/// A `UniqueIndex` is an index that is unique within the generic [`Scope`] `S`.
///
/// This allows you to compute offsets within a buffer or shared memory to safely write to.
///
/// # Safety
/// The index must be globally unique within the [`Scope`] `S`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UniqueIndex<S: Scope> {
    index: u32,
    _phantom: PhantomData<S>,
}

impl<S: Scope> UniqueIndex<S> {
    /// Create a new [`UniqueIndex`]
    ///
    /// # Safety
    /// Index must be unique within the [`Scope`] `S`.
    #[inline]
    pub const unsafe fn new_unchecked(index: u32) -> Self {
        Self {
            index,
            _phantom: PhantomData,
        }
    }
}

impl<S: Scope> Deref for UniqueIndex<S> {
    type Target = u32;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.index
    }
}

impl<S: Scope> UniqueIndex<S> {
    /// Convert to `u32`
    pub const fn to_u32(&self) -> u32 {
        self.index
    }

    /// Convert to `usize`
    pub const fn to_usize(&self) -> usize {
        self.index as usize
    }
}

impl<S: Scope> From<UniqueIndex<S>> for u32 {
    fn from(inner: UniqueIndex<S>) -> Self {
        inner.index
    }
}

impl<S: Scope> From<UniqueIndex<S>> for usize {
    fn from(value: UniqueIndex<S>) -> Self {
        value.index as usize
    }
}

impl<T, S: Scope> Index<UniqueIndex<S>> for [T] {
    type Output = T;

    fn index(&self, index: UniqueIndex<S>) -> &Self::Output {
        self.index(index.index as usize)
    }
}

/// This is less useful as it seems. The `&mut self` guarantees exclusive access to the slice, but you really
/// want to use this type on slice-like objects that are shared between multiple invocations.
impl<T, S: Scope> IndexMut<UniqueIndex<S>> for [T] {
    fn index_mut(&mut self, index: UniqueIndex<S>) -> &mut Self::Output {
        self.index_mut(index.index as usize)
    }
}

macro_rules! impl_downcast {
    ($name:ident, $from:ident, $to:ident) => {
        impl<S: $from> UniqueIndex<S> {
            #[doc = concat!("Downcast this [`UniqueIndex`] to the [`", stringify!($to), "`] scope")]
            pub fn $name(&self) -> UniqueIndex<$to> {
                // Safety: downcasting scopes to one with fewer guarantees is safe
                unsafe { UniqueIndex::new_unchecked(self.index) }
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
