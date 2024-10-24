//! Definitions of `RangedWrapping<T, U>`.

use std::cmp::{PartialEq, PartialOrd};
use std::fmt;
use std::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div, DivAssign, Mul, MulAssign, Neg, Not,
    Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

use forward_ref_generic::{forward_ref_binop, forward_ref_op_assign};

//TODO: fix this documenation
/// Provides intentionally-wrapped arithmetic on `T`.
///
/// Operations like `+` on `u32` values are intended to never overflow,
/// and in some debug configurations overflow is detected and results
/// in a panic. While most arithmetic falls into this category, some
/// code explicitly expects and relies upon modular arithmetic (e.g.,
/// hashing).
///
/// Wrapping arithmetic can be achieved either through methods like
/// `wrapping_add`, or through the `Wrapping<T>` type, which says that
/// all standard arithmetic operations on the underlying value are
/// intended to have wrapping semantics.
///
/// The underlying value can be retrieved through the `.0` index of the
/// `Wrapping` tuple.
///
/// # Examples
///
/// ```
/// use std::num::Wrapping;
///
/// let zero = Wrapping(0u32);
/// let one = Wrapping(1u32);
///
/// assert_eq!(u32::MAX, (zero - one).0);
/// ```
///
/// # Layout
///
/// `Wrapping<T>` is guaranteed to have the same layout and ABI as `T`.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default, Hash)]
pub struct RangedWrapping<T, U>(pub T, pub U, pub U);

//https://stackoverflow.com/a/14416133/3342767
//
//https://users.rust-lang.org/t/wrapping-a-number-around-a-designated-inclusive-range/116737/2?u=sww1235
fn wrap<T, U>(input: T, max: U, min: U) -> T
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
{
    if input <= max && input >= min {
        return input;
    }

    let range_size = max - min + 1;

    let mut temp = input;

    if temp < min {
        temp += range_size * ((Into::<T>::into(min) - temp) / range_size + 1);
    }

    Into::<T>::into(min) + (temp - min) % range_size
}

impl<T: fmt::Debug, U> fmt::Debug for RangedWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::Display, U> fmt::Display for RangedWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::Binary, U> fmt::Binary for RangedWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::Octal, U> fmt::Octal for RangedWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::LowerHex, U> fmt::LowerHex for RangedWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::UpperHex, U> fmt::UpperHex for RangedWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T, U> Add for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    type Output = RangedWrapping<T, U>;

    #[inline]
    fn add(self, other: RangedWrapping<T, U>) -> RangedWrapping<T, U> {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        RangedWrapping(wrap(self.0 + other.0, self.1, self.2), self.1, self.2)
    }
}

forward_ref_binop! {
    [T, U]
    impl Add, add for RangedWrapping<T, U>
    where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,

}

impl<T, U> AddAssign for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    #[inline]
    fn add_assign(&mut self, other: Self) {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = RangedWrapping(wrap(self.0 + other.0, self.1, self.2), self.1, self.2);
        //*self = *self + RangedWrapping(other.0, self.1, self.2);
    }
}

forward_ref_op_assign! {
    [T, U]
    impl AddAssign, add_assign for RangedWrapping<T, U>
    where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
}

impl<T, U> Sub for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    type Output = RangedWrapping<T, U>;

    #[inline]
    fn sub(self, other: RangedWrapping<T, U>) -> RangedWrapping<T, U> {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        RangedWrapping(wrap(self.0 - other.0, self.1, self.2), self.1, self.2)
    }
}
impl<T, U> SubAssign for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    #[inline]
    fn sub_assign(&mut self, other: Self) {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self - RangedWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Mul for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    type Output = RangedWrapping<T, U>;

    #[inline]
    fn mul(self, other: RangedWrapping<T, U>) -> RangedWrapping<T, U> {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        RangedWrapping(wrap(self.0 * other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> MulAssign for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    #[inline]
    fn mul_assign(&mut self, other: Self) {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self * RangedWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Div for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    type Output = RangedWrapping<T, U>;

    #[inline]
    fn div(self, other: RangedWrapping<T, U>) -> RangedWrapping<T, U> {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        RangedWrapping(wrap(self.0 / other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> DivAssign for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    #[inline]
    fn div_assign(&mut self, other: Self) {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self / RangedWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Rem for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    type Output = RangedWrapping<T, U>;

    #[inline]
    fn rem(self, other: RangedWrapping<T, U>) -> RangedWrapping<T, U> {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        RangedWrapping(wrap(self.0 % other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> RemAssign for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    #[inline]
    fn rem_assign(&mut self, other: Self) {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self % RangedWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Not for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: Not<Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
{
    type Output = RangedWrapping<T, U>;

    #[inline]
    fn not(self) -> RangedWrapping<T, U> {
        RangedWrapping(!self.0, self.1, self.2)
    }
}

impl<T, U> BitXor for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    type Output = RangedWrapping<T, U>;

    #[inline]
    fn bitxor(self, other: RangedWrapping<T, U>) -> RangedWrapping<T, U> {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        RangedWrapping(wrap(self.0 ^ other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> BitXorAssign for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitXor<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    #[inline]
    fn bitxor_assign(&mut self, other: Self) {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self ^ RangedWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> BitOr for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    type Output = RangedWrapping<T, U>;

    #[inline]
    fn bitor(self, other: RangedWrapping<T, U>) -> RangedWrapping<T, U> {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        RangedWrapping(wrap(self.0 | other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> BitOrAssign for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitOr<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    #[inline]
    fn bitor_assign(&mut self, other: Self) {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self | RangedWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> BitAnd for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    type Output = RangedWrapping<T, U>;

    #[inline]
    fn bitand(self, other: RangedWrapping<T, U>) -> RangedWrapping<T, U> {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        RangedWrapping(wrap(self.0 & other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> BitAndAssign for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    T: BitAnd<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    U: PartialEq<U>,
{
    #[inline]
    //TODO: need to make sure this actually wraps properly
    fn bitand_assign(&mut self, other: Self) {
        if self.1 != other.1 || self.2 != other.2 {
            panic!("self and other values of RangedWrapping do not have the same bounds")
        }
        *self = *self & RangedWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Neg for RangedWrapping<T, U>
where
    T: PartialOrd<T> + AddAssign,
    T: PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: Sub<T, Output = T>,
    T: Sub<U, Output = T>,
    T: Add<i32, Output = T>,
    T: Add<T, Output = T>,
    T: Add<U, Output = T>,
    T: Mul<T, Output = T>,
    T: Div<T, Output = T>,
    T: Rem<T, Output = T>,
    U: Sub<U, Output = T>,
    U: Add<i32, Output = T>,
    i32: Sub<T, Output = T>,
{
    type Output = Self;
    #[inline]
    //TODO: panic if negation would be less than lower bound
    fn neg(self) -> Self {
        let temp = 0i32 - self.0;
        if temp < self.2 {
            panic!("negative value is out of wrapped bounds");
        }
        RangedWrapping(temp, self.1, self.2)
    }
}

//TODO: fix
impl<T, U> RangedWrapping<T, U> {
    //    /// Returns the smallest value that can be represented by this type.
    //    ///
    //    //pub const MIN: U = self.1;
    //
    //    /// Returns the largest value that can be represented by this type.
    //    ///
    //    //pub const MAX: U = self.2;
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
    //            #[doc = concat!("assert_eq!(RangedWrapping(100", stringify!(T), ").abs(), RangedWrapping(100));")]
    //            #[doc = concat!("assert_eq!(RangedWrapping(-100", stringify!(T), ").abs(), RangedWrapping(100));")]
    //            #[doc = concat!("assert_eq!(RangedWrapping(", stringify!(T), "::MIN).abs(), RangedWrapping(", stringify!(T), "::MIN));")]
    //            /// assert_eq!(RangedWrapping(-128i8).abs().0 as u8, 128u8);
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub fn abs(self) -> RangedWrapping<T, U> {
    //                RangedWrapping(self.0.wrapping_abs())
    //            }
    //
    //            /// Returns a number representing sign of `self`.
    //            ///
    //            ///  - `0` if the number is zero
    //            ///  - `1` if the number is positive
    //            ///  - `-1` if the number is negative
    //            ///
    //            /// # Examples
    //            ///
    //            /// Basic usage:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            #[doc = concat!("assert_eq!(RangedWrapping(10", stringify!(T), ").signum(), RangedWrapping(1));")]
    //            #[doc = concat!("assert_eq!(RangedWrapping(0", stringify!(T), ").signum(), RangedWrapping(0));")]
    //            #[doc = concat!("assert_eq!(RangedWrapping(-10", stringify!(T), ").signum(), RangedWrapping(-1));")]
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub fn signum(self) -> RangedWrapping<T, U> {
    //                RangedWrapping(self.0.signum())
    //            }
    //
    //            /// Returns `true` if `self` is positive and `false` if the number is zero or
    //            /// negative.
    //            ///
    //            /// # Examples
    //            ///
    //            /// Basic usage:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            #[doc = concat!("assert!(RangedWrapping(10", stringify!(T), ").is_positive());")]
    //            #[doc = concat!("assert!(!RangedWrapping(-10", stringify!(T), ").is_positive());")]
    //            /// ```
    //            #[must_use]
    //            #[inline]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn is_positive(self) -> bool {
    //                self.0.is_positive()
    //            }
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
    //            #[doc = concat!("assert!(RangedWrapping(-10", stringify!(T), ").is_negative());")]
    //            #[doc = concat!("assert!(!RangedWrapping(10", stringify!(T), ").is_negative());")]
    //            /// ```
    //            #[must_use]
    //            #[inline]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn is_negative(self) -> bool {
    //                self.0.is_negative()
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
    //            /// let n: RangedWrapping<i64> = RangedWrapping(0x0123456789ABCDEF);
    //            /// let m: RangedWrapping<i64> = RangedWrapping(-0x76543210FEDCBA99);
    //            ///
    //            /// assert_eq!(n.rotate_left(32), m);
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn rotate_left(self, n: u32) -> Self {
    //                RangedWrapping(self.0.rotate_left(n))
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
    //            /// let n: RangedWrapping<i64> = RangedWrapping(0x0123456789ABCDEF);
    //            /// let m: RangedWrapping<i64> = RangedWrapping(-0xFEDCBA987654322);
    //            ///
    //            /// assert_eq!(n.rotate_right(4), m);
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn rotate_right(self, n: u32) -> Self {
    //                RangedWrapping(self.0.rotate_right(n))
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
    //            /// let n: RangedWrapping<i16> = RangedWrapping(0b0000000_01010101);
    //            /// assert_eq!(n, RangedWrapping(85));
    //            ///
    //            /// let m = n.swap_bytes();
    //            ///
    //            /// assert_eq!(m, RangedWrapping(0b01010101_00000000));
    //            /// assert_eq!(m, RangedWrapping(21760));
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn swap_bytes(self) -> Self {
    //                RangedWrapping(self.0.swap_bytes())
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
    //            /// let n = RangedWrapping(0b0000000_01010101i16);
    //            /// assert_eq!(n, RangedWrapping(85));
    //            ///
    //            /// let m = n.reverse_bits();
    //            ///
    //            /// assert_eq!(m.0 as u16, 0b10101010_00000000);
    //            /// assert_eq!(m, RangedWrapping(-22016));
    //            /// ```
    //            #[stable(feature = "reverse_bits", since = "1.37.0")]
    //            #[rustc_const_stable(feature = "const_reverse_bits", since = "1.37.0")]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[inline]
    //            pub const fn reverse_bits(self) -> Self {
    //                RangedWrapping(self.0.reverse_bits())
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
    //            #[doc = concat!("assert_eq!(RangedWrapping(3", stringify!(T), ").pow(4), RangedWrapping(81));")]
    //            /// ```
    //            ///
    //            /// Results that are too large are wrapped:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::RangedWrapping;
    //            ///
    //            /// assert_eq!(RangedWrapping(3i8).pow(5), RangedWrapping(-13));
    //            /// assert_eq!(RangedWrapping(3i8).pow(6), RangedWrapping(-39));
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub fn pow(self, exp: u32) -> Self {
    //                RangedWrapping(self.0.wrapping_pow(exp))
    //            }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestStruct {
        id: u64,
        content: RangedWrapping<usize, usize>,
    }

    // testing basic wrap function
    #[test]
    fn test_wrap() {
        //fn wrap<T, U>(input: T, max: U, min: U) -> T
        assert_eq!(wrap(10, 5, 2), 2);
        assert_eq!(wrap(10, 10, 2), 10);
        assert_eq!(wrap(-2, 10, 2), 7);
    }

    // testing addition
    #[test]
    fn test_addition() {
        let test1 = RangedWrapping(5, 10, 2);
        let test2 = RangedWrapping(7, 10, 2);
        let test3 = test1 + test2;
        assert_eq!(test3.0, 3);
    }

    #[test]
    fn test_addassign() {
        let mut test1 = RangedWrapping(5, 10, 2);
        test1 += RangedWrapping(7, 10, 2);
        assert_eq!(test1.0, 3);
    }

    #[test]
    fn test_addassign_struct() {
        let mut test1 = TestStruct {
            id: 0,
            content: RangedWrapping(5, 10, 2),
        };
        test1.content += RangedWrapping(7, 10, 2);
        assert_eq!(test1.content.0, 3);
    }
}
