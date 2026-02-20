use crate::unique::{
    ActiveInvocations, AtLeastActiveInvocations, AtLeastGlobal, AtLeastSubgroup, AtLeastWorkgroup,
    Global, ScalarValue, Scope, Subgroup, Workgroup,
};
use core::cmp::Ordering;
use core::fmt::{Debug, Formatter};
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::ops::{
    Add, AddAssign, Deref, Index, IndexMut, Mul, MulAssign, Shl, ShlAssign, Sub, SubAssign,
};

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

impl<S: Scope> Copy for UniqueIndex<S> {}

impl<S: Scope> Clone for UniqueIndex<S> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _phantom: PhantomData,
        }
    }
}

impl<S: Scope> Debug for UniqueIndex<S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("UniqueIndex").field(&self.index).finish()
    }
}

impl<S: Scope> Eq for UniqueIndex<S> {}

impl<S: Scope> PartialEq for UniqueIndex<S> {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&self.index, &other.index)
    }
}

impl<S: Scope> Ord for UniqueIndex<S> {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&self.index, &other.index)
    }
}

impl<S: Scope> PartialOrd for UniqueIndex<S> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        PartialOrd::partial_cmp(&self.index, &other.index)
    }
}

impl<S: Scope> Hash for UniqueIndex<S> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.index, state)
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

macro_rules! impl_op_binary {
    ($op_trait:ident, $op_fn:ident, $op_assign_trait:ident, $op_assign_fn:ident) => {
        impl<S: Scope> $op_trait<ScalarValue<u32, S>> for UniqueIndex<S> {
            type Output = UniqueIndex<S>;

            #[inline]
            fn $op_fn(self, rhs: ScalarValue<u32, S>) -> Self::Output {
                unsafe { Self::new_unchecked($op_trait::$op_fn(self.index, rhs.into_inner())) }
            }
        }

        impl<S: Scope> $op_assign_trait<ScalarValue<u32, S>> for UniqueIndex<S> {
            #[inline]
            fn $op_assign_fn(&mut self, rhs: ScalarValue<u32, S>) {
                *self = $op_trait::$op_fn(*self, rhs);
            }
        }
    };
}

// only operation that do not degrade uniqueness (assuming they don't overflow)
impl_op_binary!(Add, add, AddAssign, add_assign);
impl_op_binary!(Sub, sub, SubAssign, sub_assign);
impl_op_binary!(Mul, mul, MulAssign, mul_assign);
impl_op_binary!(Shl, shl, ShlAssign, shl_assign);
