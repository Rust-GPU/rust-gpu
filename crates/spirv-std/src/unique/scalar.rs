use crate::unique::Scope;
use core::marker::PhantomData;
use core::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Deref, Div,
    DivAssign, Mul, MulAssign, Not, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

/// A [`ScalarValue`] is a value of type `T` that is "scalar" across some [`Scope`] `S`, meaning that it is the same
/// value for every invocation. Use the macro [`crate::const_scalar`]`!(expr)` to declare [`ScalarValue`] constants.
///
/// [`UniqueIndex`] offers a limited set of math operations with [`ScalarValue`] that guarantee to retain uniqueness.
pub struct ScalarValue<T, S: Scope> {
    value: T,
    _phantom: PhantomData<S>,
}

impl<T, S: Scope> ScalarValue<T, S> {
    /// Create a new [`ScalarValue`]
    ///
    /// # Safety
    /// The provided `value` must be scalar across the scope `S`.
    #[inline]
    pub unsafe fn new(value: T) -> Self {
        Self {
            value,
            _phantom: PhantomData,
        }
    }

    /// Return a reference to the inner value
    #[inline]
    pub fn to_inner(&self) -> &T {
        &self.value
    }

    /// Return the inner value
    #[inline]
    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T, S: Scope> Deref for ScalarValue<T, S> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

/// Create a [`ScalarValue`] from a constant
#[macro_export]
macro_rules! const_scalar {
    ($expr:expr) => {
        unsafe { $crate::entry::ScalarValue::<_, $crate::entry::Global>::new(const { $expr }) }
    };
}

/// Types that retain their equality when math operations are run on them, including casts using [`From`].
///
/// This enables math operations when they are wrapped by [`ScalarValue`].
pub unsafe trait ScalarValueMath {}

unsafe impl ScalarValueMath for u8 {}
unsafe impl ScalarValueMath for u16 {}
unsafe impl ScalarValueMath for u32 {}
unsafe impl ScalarValueMath for u64 {}
unsafe impl ScalarValueMath for usize {}
unsafe impl ScalarValueMath for i8 {}
unsafe impl ScalarValueMath for i16 {}
unsafe impl ScalarValueMath for i32 {}
unsafe impl ScalarValueMath for i64 {}
unsafe impl ScalarValueMath for isize {}
unsafe impl ScalarValueMath for glam::UVec2 {}
unsafe impl ScalarValueMath for glam::UVec3 {}
unsafe impl ScalarValueMath for glam::UVec4 {}
unsafe impl ScalarValueMath for glam::IVec2 {}
unsafe impl ScalarValueMath for glam::IVec3 {}
unsafe impl ScalarValueMath for glam::IVec4 {}

impl<T: ScalarValueMath + Not, S: Scope> Not for ScalarValue<T, S> {
    type Output = ScalarValue<T::Output, S>;

    #[inline]
    fn not(self) -> Self::Output {
        unsafe { Self::Output::new(T::not(self.value)) }
    }
}

impl<T: ScalarValueMath, S: Scope> ScalarValue<T, S> {
    /// Converts the inner type of [`ScalarValue`] using the [`From`] trait.
    ///
    /// Can't `impl From` since it conflicts with `impl<T> From<T> for T;`.
    pub fn from<F>(value: ScalarValue<F, S>) -> Self
    where
        T: From<F>,
    {
        unsafe { ScalarValue::new(From::from(value.value)) }
    }
}

macro_rules! impl_op_binary {
    ($op_trait:ident, $op_fn:ident, $op_assign_trait:ident, $op_assign_fn:ident) => {
        impl<T: ScalarValueMath + $op_trait<Rhs>, Rhs: ScalarValueMath, S: Scope>
            $op_trait<ScalarValue<Rhs, S>> for ScalarValue<T, S>
        {
            type Output = ScalarValue<T::Output, S>;

            #[inline]
            fn $op_fn(self, rhs: ScalarValue<Rhs, S>) -> Self::Output {
                unsafe { Self::Output::new(T::$op_fn(self.value, rhs.value)) }
            }
        }

        impl<T: ScalarValueMath + $op_assign_trait<Rhs>, Rhs: ScalarValueMath, S: Scope>
            $op_assign_trait<ScalarValue<Rhs, S>> for ScalarValue<T, S>
        {
            #[inline]
            fn $op_assign_fn(&mut self, rhs: ScalarValue<Rhs, S>) {
                $op_assign_trait::$op_assign_fn(&mut self.value, rhs.value);
            }
        }
    };
}

impl_op_binary!(Add, add, AddAssign, add_assign);
impl_op_binary!(Sub, sub, SubAssign, sub_assign);
impl_op_binary!(Mul, mul, MulAssign, mul_assign);
impl_op_binary!(Div, div, DivAssign, div_assign);
impl_op_binary!(Rem, rem, RemAssign, rem_assign);
impl_op_binary!(BitAnd, bitand, BitAndAssign, bitand_assign);
impl_op_binary!(BitOr, bitor, BitOrAssign, bitor_assign);
impl_op_binary!(BitXor, bitxor, BitXorAssign, bitxor_assign);
impl_op_binary!(Shr, shr, ShrAssign, shr_assign);
impl_op_binary!(Shl, shl, ShlAssign, shl_assign);
