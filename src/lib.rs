//! Definitions of `ArbitraryWrapping<T, U>`.

use std::cmp::PartialOrd;
use std::fmt;
use std::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div, DivAssign, Mul, MulAssign, Neg, Not,
    Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

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
pub struct ArbitraryWrapping<T, U>(T, U, U);

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

impl<T: fmt::Debug, U> fmt::Debug for ArbitraryWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::Display, U> fmt::Display for ArbitraryWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::Binary, U> fmt::Binary for ArbitraryWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::Octal, U> fmt::Octal for ArbitraryWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::LowerHex, U> fmt::LowerHex for ArbitraryWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: fmt::UpperHex, U> fmt::UpperHex for ArbitraryWrapping<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

//TODO: need to make sure that the max/min values are the same on any of these operations
impl<T, U> Add for ArbitraryWrapping<T, U>
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
    type Output = ArbitraryWrapping<T, U>;

    #[inline]
    fn add(self, other: ArbitraryWrapping<T, U>) -> ArbitraryWrapping<T, U> {
        ArbitraryWrapping(wrap(self.0 + other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> AddAssign for ArbitraryWrapping<T, U>
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
    #[inline]
    fn add_assign(&mut self, other: Self) {
        *self = *self + ArbitraryWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Sub for ArbitraryWrapping<T, U>
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
    type Output = ArbitraryWrapping<T, U>;

    #[inline]
    fn sub(self, other: ArbitraryWrapping<T, U>) -> ArbitraryWrapping<T, U> {
        ArbitraryWrapping(wrap(self.0 - other.0, self.1, self.2), self.1, self.2)
    }
}
impl<T, U> SubAssign for ArbitraryWrapping<T, U>
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
    #[inline]
    fn sub_assign(&mut self, other: Self) {
        *self = *self - ArbitraryWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Mul for ArbitraryWrapping<T, U>
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
    type Output = ArbitraryWrapping<T, U>;

    #[inline]
    fn mul(self, other: ArbitraryWrapping<T, U>) -> ArbitraryWrapping<T, U> {
        ArbitraryWrapping(wrap(self.0 * other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> MulAssign for ArbitraryWrapping<T, U>
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
    #[inline]
    fn mul_assign(&mut self, other: Self) {
        *self = *self * ArbitraryWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Div for ArbitraryWrapping<T, U>
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
    type Output = ArbitraryWrapping<T, U>;

    #[inline]
    fn div(self, other: ArbitraryWrapping<T, U>) -> ArbitraryWrapping<T, U> {
        ArbitraryWrapping(wrap(self.0 / other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> DivAssign for ArbitraryWrapping<T, U>
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
    #[inline]
    fn div_assign(&mut self, other: Self) {
        *self = *self / ArbitraryWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Rem for ArbitraryWrapping<T, U>
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
    type Output = ArbitraryWrapping<T, U>;

    #[inline]
    fn rem(self, other: ArbitraryWrapping<T, U>) -> ArbitraryWrapping<T, U> {
        ArbitraryWrapping(wrap(self.0 % other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> RemAssign for ArbitraryWrapping<T, U>
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
    #[inline]
    fn rem_assign(&mut self, other: Self) {
        *self = *self % ArbitraryWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Not for ArbitraryWrapping<T, U>
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
    type Output = ArbitraryWrapping<T, U>;

    #[inline]
    fn not(self) -> ArbitraryWrapping<T, U> {
        ArbitraryWrapping(!self.0, self.1, self.2)
    }
}

impl<T, U> BitXor for ArbitraryWrapping<T, U>
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
{
    type Output = ArbitraryWrapping<T, U>;

    #[inline]
    fn bitxor(self, other: ArbitraryWrapping<T, U>) -> ArbitraryWrapping<T, U> {
        ArbitraryWrapping(wrap(self.0 ^ other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> BitXorAssign for ArbitraryWrapping<T, U>
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
{
    #[inline]
    fn bitxor_assign(&mut self, other: Self) {
        *self = *self ^ ArbitraryWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> BitOr for ArbitraryWrapping<T, U>
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
{
    type Output = ArbitraryWrapping<T, U>;

    #[inline]
    fn bitor(self, other: ArbitraryWrapping<T, U>) -> ArbitraryWrapping<T, U> {
        ArbitraryWrapping(wrap(self.0 | other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> BitOrAssign for ArbitraryWrapping<T, U>
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
{
    #[inline]
    fn bitor_assign(&mut self, other: Self) {
        *self = *self | ArbitraryWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> BitAnd for ArbitraryWrapping<T, U>
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
{
    type Output = ArbitraryWrapping<T, U>;

    #[inline]
    fn bitand(self, other: ArbitraryWrapping<T, U>) -> ArbitraryWrapping<T, U> {
        ArbitraryWrapping(wrap(self.0 & other.0, self.1, self.2), self.1, self.2)
    }
}

impl<T, U> BitAndAssign for ArbitraryWrapping<T, U>
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
{
    #[inline]
    //TODO: need to make sure this actually wraps properly
    fn bitand_assign(&mut self, other: Self) {
        *self = *self & ArbitraryWrapping(other.0, self.1, self.2);
    }
}

impl<T, U> Neg for ArbitraryWrapping<T, U>
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
        ArbitraryWrapping(temp, self.1, self.2)
    }
}

//TODO: fix
impl<T, U> ArbitraryWrapping<T, U> {
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
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            #[doc = concat!("assert_eq!(ArbitraryWrapping(100", stringify!(T), ").abs(), ArbitraryWrapping(100));")]
    //            #[doc = concat!("assert_eq!(ArbitraryWrapping(-100", stringify!(T), ").abs(), ArbitraryWrapping(100));")]
    //            #[doc = concat!("assert_eq!(ArbitraryWrapping(", stringify!(T), "::MIN).abs(), ArbitraryWrapping(", stringify!(T), "::MIN));")]
    //            /// assert_eq!(ArbitraryWrapping(-128i8).abs().0 as u8, 128u8);
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub fn abs(self) -> ArbitraryWrapping<T, U> {
    //                ArbitraryWrapping(self.0.wrapping_abs())
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
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            #[doc = concat!("assert_eq!(ArbitraryWrapping(10", stringify!(T), ").signum(), ArbitraryWrapping(1));")]
    //            #[doc = concat!("assert_eq!(ArbitraryWrapping(0", stringify!(T), ").signum(), ArbitraryWrapping(0));")]
    //            #[doc = concat!("assert_eq!(ArbitraryWrapping(-10", stringify!(T), ").signum(), ArbitraryWrapping(-1));")]
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub fn signum(self) -> ArbitraryWrapping<T, U> {
    //                ArbitraryWrapping(self.0.signum())
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
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            #[doc = concat!("assert!(ArbitraryWrapping(10", stringify!(T), ").is_positive());")]
    //            #[doc = concat!("assert!(!ArbitraryWrapping(-10", stringify!(T), ").is_positive());")]
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
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            #[doc = concat!("assert!(ArbitraryWrapping(-10", stringify!(T), ").is_negative());")]
    //            #[doc = concat!("assert!(!ArbitraryWrapping(10", stringify!(T), ").is_negative());")]
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
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            /// let n: ArbitraryWrapping<i64> = ArbitraryWrapping(0x0123456789ABCDEF);
    //            /// let m: ArbitraryWrapping<i64> = ArbitraryWrapping(-0x76543210FEDCBA99);
    //            ///
    //            /// assert_eq!(n.rotate_left(32), m);
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn rotate_left(self, n: u32) -> Self {
    //                ArbitraryWrapping(self.0.rotate_left(n))
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
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            /// let n: ArbitraryWrapping<i64> = ArbitraryWrapping(0x0123456789ABCDEF);
    //            /// let m: ArbitraryWrapping<i64> = ArbitraryWrapping(-0xFEDCBA987654322);
    //            ///
    //            /// assert_eq!(n.rotate_right(4), m);
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn rotate_right(self, n: u32) -> Self {
    //                ArbitraryWrapping(self.0.rotate_right(n))
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
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            /// let n: ArbitraryWrapping<i16> = ArbitraryWrapping(0b0000000_01010101);
    //            /// assert_eq!(n, ArbitraryWrapping(85));
    //            ///
    //            /// let m = n.swap_bytes();
    //            ///
    //            /// assert_eq!(m, ArbitraryWrapping(0b01010101_00000000));
    //            /// assert_eq!(m, ArbitraryWrapping(21760));
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub const fn swap_bytes(self) -> Self {
    //                ArbitraryWrapping(self.0.swap_bytes())
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
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            /// let n = ArbitraryWrapping(0b0000000_01010101i16);
    //            /// assert_eq!(n, ArbitraryWrapping(85));
    //            ///
    //            /// let m = n.reverse_bits();
    //            ///
    //            /// assert_eq!(m.0 as u16, 0b10101010_00000000);
    //            /// assert_eq!(m, ArbitraryWrapping(-22016));
    //            /// ```
    //            #[stable(feature = "reverse_bits", since = "1.37.0")]
    //            #[rustc_const_stable(feature = "const_reverse_bits", since = "1.37.0")]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[inline]
    //            pub const fn reverse_bits(self) -> Self {
    //                ArbitraryWrapping(self.0.reverse_bits())
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
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            #[doc = concat!("assert_eq!(ArbitraryWrapping(3", stringify!(T), ").pow(4), ArbitraryWrapping(81));")]
    //            /// ```
    //            ///
    //            /// Results that are too large are wrapped:
    //            ///
    //            /// ```
    //            /// #![feature(wrapping_int_impl)]
    //            /// use std::num::ArbitraryWrapping;
    //            ///
    //            /// assert_eq!(ArbitraryWrapping(3i8).pow(5), ArbitraryWrapping(-13));
    //            /// assert_eq!(ArbitraryWrapping(3i8).pow(6), ArbitraryWrapping(-39));
    //            /// ```
    //            #[inline]
    //            #[must_use = "this returns the result of the operation, \
    //                          without modifying the original"]
    //            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
    //            pub fn pow(self, exp: u32) -> Self {
    //                ArbitraryWrapping(self.0.wrapping_pow(exp))
    //            }
}

#[cfg(test)]
mod tests {
    use super::*;

    // testing basic wrap function
    #[test]
    fn test_wrap() {
        //fn wrap<T, U>(input: T, max: U, min: U) -> T
        assert_eq!(wrap(10, 5, 2), 2);
        assert_eq!(wrap(10, 10, 2), 10);
        assert_eq!(wrap(-2, 10, 2), 7);
    }
}
