//! Definitions of `RangedWrapping<T>`.

use std::cmp::{PartialEq, PartialOrd};
use std::fmt::{self, Display};
#[expect(unused_imports)]
use std::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div, DivAssign, Mul, MulAssign, Neg, Not,
    Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

use forward_ref_generic::{forward_ref_binop, forward_ref_op_assign, forward_ref_unop};
#[expect(unused_imports)]
use num_traits::{
    bounds::Bounded,
    identities::{One, Zero},
    ops::checked::{CheckedAdd, CheckedDiv, CheckedMul, CheckedNeg, CheckedRem, CheckedShl, CheckedShr, CheckedSub},
    Signed,
};

//TODO: use num_traits to implement math against primative types

/// Provides intentionally-wrapped arithmetic on `T` within a defined range.
///
/// This type builds on [`std::num::Wrapping<T>`] by allowing you to specify a maximum and minimum
/// range to wrap the value around. This is useful in most places you would traditionally use
/// modular arithmatic to wrap a value around a range that is smaller than your datatype width.
///
/// The maximum and minimum values are both inclusive.
///
/// All standard stable math operations defined on [`std::num::Wrapping<T>`] are also defined on
/// `RangedWrapping<T>`.
///
/// The underlying value can be retrieved through `.value`.
///
/// Note: Two `RangedWrapping` instance with different max or min values are considered
/// different types and prevented from being mathed together. Unfortunately Rust doesn't have
/// dependant or refinement types (which would technically eliminate the need for this entire
/// library) so this must be done at run time with panics.
///
/// # Examples
///
/// See tests for more examples
///
/// ```
/// use ranged_wrapping::RangedWrapping;
///
/// let test1 = RangedWrapping {
///    value: 5,
///    max: 10,
///    min: 2,
/// };
/// let test2 = RangedWrapping {
///    value: 7,
///    max: 10,
///    min: 2,
/// };
/// let test3 = test1 + test2;
/// assert_eq!(test3.value, 3);
/// ```
///
/// # Panics
///
/// Any math operation on two instances of `RangedWrapping` will panic where the max or min values
/// are not equal.

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default, Hash)]
pub struct RangedWrapping<T> {
    pub value: T,
    pub max: T,
    pub min: T,
}

//https://stackoverflow.com/a/14416133/3342767
//
//https://users.rust-lang.org/t/wrapping-a-number-around-a-designated-inclusive-range/116737/2?u=sww1235
//
//https://users.rust-lang.org/t/confusion-on-implementing-add-assign-on-struct/120173/2
//
//TODO: check to make sure these traits are all needed
fn wrap<T>(input: T, max: T, min: T) -> T
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    if input <= max && input >= min {
        return input;
    }

    let range_size = max - min + T::one();

    let mut temp = input;

    if temp < min {
        temp = temp + range_size * ((Into::<T>::into(min) - temp) / range_size + T::one());
    }

    Into::<T>::into(min) + (temp - min) % range_size
}

impl<T: fmt::Binary> fmt::Binary for RangedWrapping<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: fmt::Octal> fmt::Octal for RangedWrapping<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: fmt::LowerHex> fmt::LowerHex for RangedWrapping<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: fmt::UpperHex> fmt::UpperHex for RangedWrapping<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T> Add<RangedWrapping<T>> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedAdd,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn add(self, other: Self) -> Self {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds when adding")
        }
        if let Some(checked_value) = self.value.checked_add(&other.value) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value + other.value is greater than T::MAX, so wrap around to T:MIN value
        } else {
            Self {
                value: wrap(T::min_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Add, add for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedAdd,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Add<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedAdd,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn add(self, other: T) -> Self {
        if let Some(checked_value) = self.value.checked_add(&other) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value + other.value is greater than T::MAX, so wrap around to T:MIN value
        } else {
            Self {
                value: wrap(T::min_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Add, add for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedAdd,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> AddAssign<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedAdd,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn add_assign(&mut self, other: Self) {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds when add_assign")
        }
        *self = *self + other;
    }
}
forward_ref_op_assign! {
    [T]
    impl AddAssign, add_assign for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedAdd,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> AddAssign<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedAdd,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn add_assign(&mut self, other: T) {
        *self = *self + other;
    }
}

forward_ref_op_assign! {
    [T]
    impl AddAssign, add_assign for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedAdd,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Sub<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedSub,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn sub(self, other: Self) -> Self {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        if let Some(checked_value) = self.value.checked_sub(&other.value) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value - other.value is less than T::MIN, so wrap around to T:MAX value
        } else {
            Self {
                value: wrap(T::max_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Sub, sub for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedSub,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Sub<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedSub,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn sub(self, other: T) -> Self {
        if let Some(checked_value) = self.value.checked_sub(&other) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value - other is less than T::MIN, so wrap around to T:MAX value
        } else {
            Self {
                value: wrap(T::max_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Sub, sub for RangedWrapping<T>,T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedSub,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> SubAssign<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedSub,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn sub_assign(&mut self, other: Self) {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self - other;
    }
}

forward_ref_op_assign! {
    [T]
    impl SubAssign, sub_assign for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedSub,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> SubAssign<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedSub,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn sub_assign(&mut self, other: T) {
        *self = *self - other;
    }
}

forward_ref_op_assign! {
    [T]
    impl SubAssign, sub_assign for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedSub,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Mul<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedMul,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn mul(self, other: Self) -> Self {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        if let Some(checked_value) = self.value.checked_mul(&other.value) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value * other.value is greater than T::MAX, so wrap around to T:MIN value
        } else {
            Self {
                value: wrap(T::min_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Mul, mul for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedMul,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Mul<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedMul,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn mul(self, other: T) -> Self {
        if let Some(checked_value) = self.value.checked_mul(&other) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value * other is greater than T::MAX, so wrap around to T:MIN value
        } else {
            Self {
                value: wrap(T::min_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Mul, mul for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedMul,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> MulAssign<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedMul,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn mul_assign(&mut self, other: Self) {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self * other;
    }
}

forward_ref_op_assign! {
    [T]
    impl MulAssign, mul_assign for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedMul,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> MulAssign<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedMul,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn mul_assign(&mut self, other: T) {
        *self = *self * other;
    }
}

forward_ref_op_assign! {
    [T]
    impl MulAssign, mul_assign for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedMul,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Div<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedDiv,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn div(self, other: Self) -> Self {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        if let Some(checked_value) = self.value.checked_div(&other.value) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value / other.value is less than T::MIN, so wrap around to T:MAX value
        } else {
            Self {
                value: wrap(T::max_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Div, div for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedDiv,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Div<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedDiv,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn div(self, other: T) -> Self {
        if let Some(checked_value) = self.value.checked_div(&other) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value / other is less than T::MIN, so wrap around to T:MAX value
        } else {
            Self {
                value: wrap(T::max_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Div, div for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedDiv,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> DivAssign<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedDiv,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn div_assign(&mut self, other: Self) {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self / other;
    }
}

forward_ref_op_assign! {
    [T]
    impl DivAssign, div_assign for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedDiv,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> DivAssign<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedDiv,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn div_assign(&mut self, other: T) {
        *self = *self / other;
    }
}

forward_ref_op_assign! {
    [T]
    impl DivAssign, div_assign for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedDiv,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Rem<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedRem,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn rem(self, other: Self) -> Self {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        if let Some(checked_value) = self.value.checked_rem(&other.value) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value % other.value is less than T::MIN, so wrap around to T:MAX value
        } else {
            Self {
                value: wrap(T::max_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Rem, rem for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedRem,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Rem<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedRem,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn rem(self, other: T) -> Self {
        if let Some(checked_value) = self.value.checked_rem(&other) {
            Self {
                value: wrap(checked_value, self.max, self.min),
                max: self.max,
                min: self.min,
            }
            // self.value % other is less than T::MIN, so wrap around to T:MAX value
        } else {
            Self {
                value: wrap(T::max_value(), self.max, self.min),
                max: self.max,
                min: self.min,
            }
        }
    }
}

forward_ref_binop! {
    [T]
    impl Rem, rem for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedRem,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> RemAssign<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedRem,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn rem_assign(&mut self, other: Self) {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self % other;
    }
}

forward_ref_op_assign! {
    [T]
    impl RemAssign, rem_assign for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedRem,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> RemAssign<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedRem,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
{
    #[inline]
    fn rem_assign(&mut self, other: T) {
        *self = *self % other;
    }
}

forward_ref_op_assign! {
    [T]
    impl RemAssign, rem_assign for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: CheckedRem,
    T: Bounded,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: One,
}

impl<T> Not for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: Not<Output = T>,
{
    type Output = Self;

    #[inline]
    fn not(self) -> RangedWrapping<T> {
        RangedWrapping {
            value: !self.value,
            max: self.max,
            min: self.min,
        }
    }
}

forward_ref_unop! {
    [T]
    impl Not, not for RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: Not<Output = T>,
}

impl<T> BitXor<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn bitxor(self, other: Self) -> Self {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        Self {
            value: wrap(self.value ^ other.value, self.max, self.min),
            max: self.max,
            min: self.min,
        }
    }
}

forward_ref_binop! {
    [T]
    impl BitXor, bitxor for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    T: One,
}

impl<T> BitXor<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn bitxor(self, other: T) -> Self {
        Self {
            value: wrap(self.value ^ other, self.max, self.min),
            max: self.max,
            min: self.min,
        }
    }
}

forward_ref_binop! {
    [T]
    impl BitXor, bitxor for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    T: One,
}

impl<T> BitXorAssign<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    T: One,
{
    #[inline]
    fn bitxor_assign(&mut self, other: Self) {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self ^ other;
    }
}

forward_ref_op_assign! {
    [T]
    impl BitXorAssign, bitxor_assign for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    T: One,
}

impl<T> BitXorAssign<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    T: One,
{
    #[inline]
    fn bitxor_assign(&mut self, other: T) {
        *self = *self ^ other;
    }
}

forward_ref_op_assign! {
    [T]
    impl BitXorAssign, bitxor_assign for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    T: One,
}

impl<T> BitOr<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn bitor(self, other: Self) -> Self {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        Self {
            value: wrap(self.value | other.value, self.max, self.min),
            max: self.max,
            min: self.min,
        }
    }
}

forward_ref_binop! {
    [T]
    impl BitOr, bitor for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    T: One,
}

impl<T> BitOr<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn bitor(self, other: T) -> Self {
        Self {
            value: wrap(self.value | other, self.max, self.min),
            max: self.max,
            min: self.min,
        }
    }
}

forward_ref_binop! {
    [T]
    impl BitOr, bitor for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    T: One,
}

impl<T> BitOrAssign<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    T: One,
{
    #[inline]
    fn bitor_assign(&mut self, other: Self) {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self | other;
    }
}

forward_ref_op_assign! {
    [T]
    impl BitOrAssign, bitor_assign for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    T: One,
}

impl<T> BitOrAssign<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    T: One,
{
    #[inline]
    fn bitor_assign(&mut self, other: T) {
        *self = *self | other;
    }
}

forward_ref_op_assign! {
    [T]
    impl BitOrAssign, bitor_assign for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    T: One,
}

impl<T> BitAnd<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn bitand(self, other: Self) -> Self {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        Self {
            value: wrap(self.value & other.value, self.max, self.min),
            max: self.max,
            min: self.min,
        }
    }
}

forward_ref_binop! {
    [T]
    impl BitAnd, bitand for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    T: One,
}

impl<T> BitAnd<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    T: One,
{
    type Output = Self;

    #[inline]
    fn bitand(self, other: T) -> Self {
        Self {
            value: wrap(self.value & other, self.max, self.min),
            max: self.max,
            min: self.min,
        }
    }
}

forward_ref_binop! {
    [T]
    impl BitAnd, bitand for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    T: One,
}

impl<T> BitAndAssign<Self> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    T: One,
{
    #[inline]
    //TODO: need to make sure this actually wraps properly
    fn bitand_assign(&mut self, other: Self) {
        if self.max != other.max || self.min != other.min {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self & other;
    }
}

forward_ref_op_assign! {
    [T]
    impl BitAndAssign, bitand_assign for RangedWrapping<T>, RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    T: One,
}

impl<T> BitAndAssign<T> for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    T: One,
{
    #[inline]
    //TODO: need to make sure this actually wraps properly
    fn bitand_assign(&mut self, other: T) {
        *self = *self & other;
    }
}

forward_ref_op_assign! {
    [T]
    impl BitAndAssign, bitand_assign for RangedWrapping<T>, T
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    T: One,
}

impl<T> Neg for RangedWrapping<T>
where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: Zero,
{
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        let temp = T::zero() - self.value;
        if temp < self.min {
            panic!("negative value is out of wrapped bounds");
        }
        Self {
            value: temp,
            max: self.max,
            min: self.min,
        }
    }
}

forward_ref_unop! {
    [T]
    impl Neg, neg for RangedWrapping<T>
    where
    T: PartialOrd<T>,
    T: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Add<T, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: Zero,
}

//TODO: fix
impl<T> RangedWrapping<T> {
    //    /// Returns the smallest value that can be represented by this type.
    //    ///
    //    //pub const MIN: U = self.min;
    //
    //    /// Returns the largest value that can be represented by this type.
    //    ///
    //    //pub const MAX: U = self.max;
    //            /// Computes the absolute value of `self`, wrapping around at
    //            /// the boundary of the type.
    //            ///
    //            /// The only case where such wrapping can occur is when one takes the absolute value of the negative
    //            /// minimal value for the type this is a positive value that is too large to represent in the type. In
    //            /// such a case, this function returns `MIN` itself.
    //            ///
    //            /// # Examples
    //            ///
    //            /// Basic usage:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            #[doc = concat!("assert_eq!(RangedWrapping{100", stringify!(T), ").abs(), RangedWrapping(100));"}]
    //            #[doc = concat!("assert_eq!(RangedWrapping{-100", stringify!(T), ").abs(), RangedWrapping(100));"}]
    //            #[doc = concat!("assert_eq!(RangedWrapping{", stringify!(T), "::MIN).abs(), RangedWrapping(", stringify!(T), "::MIN));"}]
    //            /// assert_eq!(RangedWrapping{-128i8).abs().0 as u8, 128u8};
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub fn abs(self) -> RangedWrapping<T> {
    //                RangedWrapping{self.value.wrapping_abs()}
    //            }
    //
    /// Returns a number representing sign of `self`.
    ///
    ///  - `0` if the number is zero
    ///  - `1` if the number is positive
    ///  - `-1` if the number is negative
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use ranged_wrapping::RangedWrapping;
    ///assert_eq!(RangedWrapping{value: 10, max: 30, min: -30}.signum(), 1);
    ///assert_eq!(RangedWrapping{value: 0, max: 30, min: -30}.signum(), 0);
    ///assert_eq!(RangedWrapping{value: -10, max: 30, min: -30}.signum(), -1);
    /// ```
    #[inline]
    #[must_use = "this returns the result of the operation, \
                              without modifying the original"]
    pub fn signum(self) -> T
    where
        T: Signed,
    {
        self.value.signum()
    }

    // /// Returns `true` if `self` is positive and `false` if the number is zero or
    // /// negative.
    // ///
    // /// # Examples
    // ///
    // /// Basic usage:
    // ///
    // /// ```
    // ///
    // ///assert!(RangedWrapping{value: 10, max: 15, min: -15}.is_positive());
    // ///assert!(!RangedWrapping{-10, max: 15, min: -15}.is_positive());
    // /// ```
    // #[must_use]
    // #[inline]
    // pub const fn is_positive(self) -> bool
    // where
    //     T: Signed,
    // {
    //     self.value.is_positive()
    // }
    //
    //            /// Returns `true` if `self` is negative and `false` if the number is zero or
    //            /// positive.
    //            ///
    //            /// # Examples
    //            ///
    //            /// Basic usage:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            #[doc = concat!("assert!(RangedWrapping{-10", stringify!(T), ").is_negative());"}]
    //            #[doc = concat!("assert!(!RangedWrapping{10", stringify!(T), ").is_negative());"}]
    //            /// ```
    //            #[must_use]
    //            #[inline]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn is_negative(self) -> bool {
    //                self.value.is_negative()
    //            }

    //
    //            /// Shifts the bits to the left by a specified amount, `n`,
    //            /// wrapping the truncated bits to the end of the resulting
    //            /// integer.
    //            ///
    //            /// Please note this isn't the same operation as the `<<` shifting
    //            /// operator!
    //            ///
    //            /// # Examples
    //            ///
    //            /// Basic usage:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            /// let n: RangedWrapping<i64> = RangedWrapping{0x0123456789ABCDEF};
    //            /// let m: RangedWrapping<i64> = RangedWrapping{-0x76543210FEDCBA99};
    //            ///
    //            /// assert_eq!(n.rotate_left(32), m);
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn rotate_left(self, n: u32) -> Self {
    //                RangedWrapping{self.value.rotate_left(n)}
    //            }
    //
    //            /// Shifts the bits to the right by a specified amount, `n`,
    //            /// wrapping the truncated bits to the beginning of the resulting
    //            /// integer.
    //            ///
    //            /// Please note this isn't the same operation as the `>>` shifting
    //            /// operator!
    //            ///
    //            /// # Examples
    //            ///
    //            /// Basic usage:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            /// let n: RangedWrapping<i64> = RangedWrapping{0x0123456789ABCDEF};
    //            /// let m: RangedWrapping<i64> = RangedWrapping{-0xFEDCBA987654322};
    //            ///
    //            /// assert_eq!(n.rotate_right(4), m);
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn rotate_right(self, n: u32) -> Self {
    //                RangedWrapping{self.value.rotate_right(n)}
    //            }
    //
    //            /// Reverses the byte order of the integer.
    //            ///
    //            /// # Examples
    //            ///
    //            /// Basic usage:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            /// let n: RangedWrapping<i16> = RangedWrapping{0b0000000_01010101};
    //            /// assert_eq!(n, RangedWrapping{85)};
    //            ///
    //            /// let m = n.swap_bytes();
    //            ///
    //            /// assert_eq!(m, RangedWrapping{0b01010101_00000000)};
    //            /// assert_eq!(m, RangedWrapping{21760)};
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn swap_bytes(self) -> Self {
    //                RangedWrapping{self.value.swap_bytes()}
    //            }
    //
    //            /// Reverses the bit pattern of the integer.
    //            ///
    //            /// # Examples
    //            ///
    //            /// Please note that this example is shared between integer types.
    //            /// Which explains why `i16` is used here.
    //            ///
    //            /// Basic usage:
    //            ///
    //            /// ```
    //            /// use std::num::RangedWrapping;
    //            ///
    //            /// let n = RangedWrapping{0b0000000_01010101i16};
    //            /// assert_eq!(n, RangedWrapping{85)};
    //            ///
    //            /// let m = n.reverse_bits();
    //            ///
    //            /// assert_eq!(m.0 as u16, 0b10101010_00000000);
    //            /// assert_eq!(m, RangedWrapping{-22016)};
    //            /// ```
    //            #[stable(feature = "reverse_bits", since = "1.37.0")]
    //            #[rustc_const_stable(feature = "const_reverse_bits", since = "1.37.0")]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[inline]
    //            pub const fn reverse_bits(self) -> Self {
    //                RangedWrapping{self.value.reverse_bits()}
    //            }
    //
    //            /// Raises self to the power of `exp`, using exponentiation by squaring.
    //            ///
    //            /// # Examples
    //            ///
    //            /// Basic usage:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            #[doc = concat!("assert_eq!(RangedWrapping{3", stringify!(T), ").pow(4), RangedWrapping(81));"}]
    //            /// ```
    //            ///
    //            /// Results that are too large are wrapped:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            /// assert_eq!(RangedWrapping{3i8).pow(5), RangedWrapping(-13)};
    //            /// assert_eq!(RangedWrapping{3i8).pow(6), RangedWrapping(-39)};
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub fn pow(self, exp: u32) -> Self {
    //                RangedWrapping{self.value.wrapping_pow(exp)}
    //            }
}

impl<T> Display for RangedWrapping<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value: {}, Max: {}, Min: {}", self.value, self.max, self.min)
    }
}

//TODO: complete tests
//
// # List of functions to test
//
//  -wrap
//      - usize
//      - f32
//      - f64
//
//  - Add RangedWrapping and RangedWrapping
//      - usize
//      - f32
//      - f64
//  - Add RangedWrapping and
//      - usize
//      - f32
//      - f64
//  - AddAssign RangedWrapping to RangedWrapping
//      - usize
//      - f32
//      - f64
//  - AddAssign RangedWrapping to
//      - usize
//      - f32
//      - f64
//  - Sub RangedWrapping and RangedWrapping
//      - usize
//      - f32
//      - f64
//  - Sub RangedWrapping and
//      - usize
//      - f32
//      - f64
//  - SubAssign RangedWrapping to RangedWrapping
//      - usize
//      - f32
//      - f64
//  - SubAssign RangedWrapping to
//      - usize
//      - f32
//      - f64
//  - Mul RangedWrapping and RangedWrapping
//      - usize
//      - f32
//      - f64
//  - Mul RangedWrapping and
//      - usize
//      - f32
//      - f64
//  - MulAssign RangedWrapping to RangedWrapping
//      - usize
//      - f32
//      - f64
//  - MulAssign RangedWrapping to
//      - usize
//      - f32
//      - f64
//  - Div RangedWrapping and RangedWrapping
//      - usize
//      - f32
//      - f64
//  - Div RangedWrapping and
//      - usize
//      - f32
//      - f64
//  - DivAssign RangedWrapping to RangedWrapping
//      - usize
//      - f32
//      - f64
//  - DivAssign RangedWrapping to
//      - usize
//      - f32
//      - f64
//  - Rem RangedWrapping and RangedWrapping
//      - usize
//      - f32
//      - f64
//  - Rem RangedWrapping and
//      - usize
//      - f32
//      - f64
//  - RemAssign RangedWrapping to RangedWrapping
//      - usize
//      - f32
//      - f64
//  - RemAssign RangedWrapping to
//      - usize
//      - f32
//      - f64
//  - Not RangedWrapping
//      - usize
//      - f32
//      - f64
//  - BitXor RangedWrapping and RangedWrapping
//      - usize
//      - f32
//      - f64
//  - BitXor RangedWrapping and
//      - usize
//      - f32
//      - f64
//  - BitXorAssign RangedWrapping to RangedWrapping
//      - usize
//      - f32
//      - f64
//  - BitXorAssign RangedWrapping to
//      - usize
//      - f32
//      - f64
//  - BitOr RangedWrapping and RangedWrapping
//      - usize
//      - f32
//      - f64
//  - BitOr RangedWrapping and
//      - usize
//      - f32
//      - f64
//  - BitOrAssign RangedWrapping to RangedWrapping
//      - usize
//      - f32
//      - f64
//  - BitOrAssign RangedWrapping to
//      - usize
//      - f32
//      - f64
//  - BitAnd RangedWrapping and RangedWrapping
//      - usize
//      - f32
//      - f64
//  - BitAnd RangedWrapping and
//      - usize
//      - f32
//      - f64
//  - BitAndAssign RangedWrapping to RangedWrapping
//      - usize
//      - f32
//      - f64
//  - BitAndAssign RangedWrapping to
//      - usize
//      - f32
//      - f64
//  - Neg RangedWrapping
//      - usize
//      - f32
//      - f64
//
#[cfg(test)]
mod tests {
    use super::*;

    struct TestStruct {
        _id: u64,
        content: RangedWrapping<usize>,
    }

    // testing basic wrap function
    #[test]
    fn test_wrap() {
        //fn wrap<T>(input: T, max: U, min: U) -> T
        assert_eq!(wrap(10, 5, 2), 2);
        assert_eq!(wrap(10, 10, 2), 10);
        assert_eq!(wrap(-2, 10, 2), 7);
    }

    // testing addition
    #[test]
    fn test_addition() {
        let test1 = RangedWrapping {
            value: 5,
            max: 10,
            min: 2,
        };
        let test2 = RangedWrapping {
            value: 7,
            max: 10,
            min: 2,
        };
        let test3 = test1 + test2;
        assert_eq!(test3.value, 3);
    }

    #[test]
    fn test_addassign() {
        let mut test1 = RangedWrapping {
            value: 5,
            max: 10,
            min: 2,
        };
        test1 += RangedWrapping {
            value: 7,
            max: 10,
            min: 2,
        };
        assert_eq!(test1.value, 3);
    }

    #[test]
    fn test_addassign_struct() {
        let mut test1 = TestStruct {
            _id: 0,
            content: RangedWrapping {
                value: 5,
                max: 10,
                min: 2,
            },
        };
        test1.content += RangedWrapping {
            value: 7,
            max: 10,
            min: 2,
        };
        assert_eq!(test1.content.value, 3);
    }

    #[test]
    fn test_add_int() {
        let test1 = RangedWrapping {
            value: 5,
            max: 10,
            min: 2,
        };

        let test2 = test1 + 2;

        assert_eq!(test2.value, 7);
    }
}
