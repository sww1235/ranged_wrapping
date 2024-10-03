//! Definitions of `ArbitraryWrapping<T, U>`.

use std::fmt;
//use std::ops::{Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign};
//use std::ops::{BitXor, BitXorAssign, Div, DivAssign};
//use std::ops::{Mul, MulAssign, Neg, Not, Rem, RemAssign};
//use std::ops::{Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign};

//use forward_ref::{forward_ref_binop, forward_ref_op_assign, forward_ref_unop};

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

#[allow(dead_code)]
//https://stackoverflow.com/a/14416133/3342767
//
//https://users.rust-lang.org/t/wrapping-a-number-around-a-designated-inclusive-range/116737/2?u=sww1235
fn wrap<T, U>(input: T, max: U, min: U) -> T
where
    T: std::cmp::PartialOrd<T> + std::ops::AddAssign,
    T: std::cmp::PartialOrd<U>,
    T: From<U>,
    T: std::marker::Copy,
    U: std::marker::Copy,
    T: std::ops::Sub<T, Output = T>,
    T: std::ops::Sub<U, Output = T>,
    T: std::ops::Add<i32, Output = T>,
    T: std::ops::Add<T, Output = T>,
    T: std::ops::Add<U, Output = T>,
    T: std::ops::Mul<T, Output = T>,
    T: std::ops::Div<T, Output = T>,
    T: std::ops::Rem<T, Output = T>,
    U: std::ops::Sub<U, Output = T>,
    U: std::ops::Add<i32, Output = T>,
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

// FIXME(30524): impl Op<T> for Wrapping<T>, impl OpAssign<T> for Wrapping<T>
#[macro_export]
macro_rules! wrapping_impl {
    ($t:ident, $u:ident, $f:ident) => {
        impl Add for ArbitraryWrapping<$t, $u> {
            type Output = ArbitraryWrapping<$t, $u>;

            #[inline]
            fn add(self, other: ArbitraryWrapping<$t, $u>) -> ArbitraryWrapping<$t, $u> {
                ArbitraryWrapping(wrap(self.0 + other.0, self.1, self.2), self.1, self.2)
            }
        }
        forward_ref_binop! { impl Add, add for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>,
        }

        impl AddAssign for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn add_assign(&mut self, other: ArbitraryWrapping<$t, $u>) {
                *self = *self + other;
            }
        }
        forward_ref_op_assign! { impl AddAssign, add_assign for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u> }

        impl AddAssign<$t> for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn add_assign(&mut self, other: $t) {
                *self = *self + ArbitraryWrapping(other, self.1, self.2);
            }
        }
        forward_ref_op_assign! { impl AddAssign, add_assign for ArbitraryWrapping<$t, $u>, $t }

        impl Sub for ArbitraryWrapping<$t, $u> {
            type Output = ArbitraryWrapping<$t, $u>;

            #[inline]
            fn sub(self, other: ArbitraryWrapping<$t, $u>) -> ArbitraryWrapping<$t, $u> {
                ArbitraryWrapping(wrap(self.0 - other.0, self.1, self.2), self.1, self.2)
            }
        }
        forward_ref_binop! { impl Sub, sub for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>,
        }

        impl SubAssign for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn sub_assign(&mut self, other: ArbitraryWrapping<$t, $u>) {
                *self = *self - other;
            }
        }
        forward_ref_op_assign! { impl SubAssign, sub_assign for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u> }

        impl SubAssign<$t> for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn sub_assign(&mut self, other: $t) {
                *self = *self - ArbitraryWrapping(other, self.1, self.2);
            }
        }
        forward_ref_op_assign! { impl SubAssign, sub_assign for ArbitraryWrapping<$t, $u>, $t }

        impl Mul for ArbitraryWrapping<$t, $u> {
            type Output = ArbitraryWrapping<$t, $u>;

            #[inline]
            fn mul(self, other: ArbitraryWrapping<$t, $u>) -> ArbitraryWrapping<$t, $u> {
                ArbitraryWrapping(wrap(self.0 * other.0, self.1, self.2), self.1, self.2)
            }
        }
        forward_ref_binop! { impl Mul, mul for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>,
        }

        impl MulAssign for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn mul_assign(&mut self, other: ArbitraryWrapping<$t, $u>) {
                *self = *self * other;
            }
        }
        forward_ref_op_assign! { impl MulAssign, mul_assign for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u> }

        impl MulAssign<$t> for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn mul_assign(&mut self, other: $t) {
                *self = *self * ArbitraryWrapping(other);
            }
        }
        forward_ref_op_assign! { impl MulAssign, mul_assign for ArbitraryWrapping<$t, $u>, $t }

        impl Div for ArbitraryWrapping<$t, $u> {
            type Output = ArbitraryWrapping<$t, $u>;

            #[inline]
            fn div(self, other: ArbitraryWrapping<$t, $u>) -> ArbitraryWrapping<$t, $u> {
                ArbitraryWrapping(wrap(self.0 / other.0, self.1, self.2), self.1, self.2)
            }
        }
        forward_ref_binop! { impl Div, div for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>,
        #[stable(feature = "wrapping_ref", since = "1.14.0")] }

        impl DivAssign for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn div_assign(&mut self, other: ArbitraryWrapping<$t, $u>) {
                *self = *self / other;
            }
        }
        forward_ref_op_assign! { impl DivAssign, div_assign for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u> }

        impl DivAssign<$t> for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn div_assign(&mut self, other: $t) {
                *self = *self / ArbitraryWrapping(other);
            }
        }
        forward_ref_op_assign! { impl DivAssign, div_assign for ArbitraryWrapping<$t, $u>, $t }

        impl Rem for ArbitraryWrapping<$t, $u> {
            type Output = ArbitraryWrapping<$t, $u>;

            #[inline]
            fn rem(self, other: ArbitraryWrapping<$t, $u>) -> ArbitraryWrapping<$t, $u> {
                ArbitraryWrapping(wrap(self.0 % other.0, self.1, self.2), self.1, self.2)
            }
        }
        forward_ref_binop! { impl Rem, rem for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>}

        impl RemAssign for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn rem_assign(&mut self, other: ArbitraryWrapping<$t, $u>) {
                *self = *self % other;
            }
        }
        forward_ref_op_assign! { impl RemAssign, rem_assign for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>}

        impl RemAssign<$t> for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn rem_assign(&mut self, other: $t) {
                *self = *self % ArbitraryWrapping(other);
            }
        }
        forward_ref_op_assign! { impl RemAssign, rem_assign for ArbitraryWrapping<$t, $u>, $t }

        impl Not for ArbitraryWrapping<$t, $u> {
            type Output = ArbitraryWrapping<$t, $u>;

            #[inline]
            fn not(self) -> ArbitraryWrapping<$t, $u> {
                ArbitraryWrapping(!self.0, self.1, self.2)
            }
        }
        forward_ref_unop! { impl Not, not for ArbitraryWrapping<$t, $u>, }

        impl BitXor for ArbitraryWrapping<$t, $u> {
            type Output = ArbitraryWrapping<$t, $u>;

            #[inline]
            fn bitxor(self, other: ArbitraryWrapping<$t, $u>) -> ArbitraryWrapping<$t, $u> {
                ArbitraryWrapping(wrap(self.0 ^ other.0, self.1, self.2), self.1, self.2)
            }
        }
        forward_ref_binop! { impl BitXor, bitxor for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>}

        impl BitXorAssign for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn bitxor_assign(&mut self, other: ArbitraryWrapping<$t, $u>) {
                *self = *self ^ other;
            }
        }
        forward_ref_op_assign! { impl BitXorAssign, bitxor_assign for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>}

        impl BitXorAssign<$t> for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn bitxor_assign(&mut self, other: $t) {
                *self = *self ^ ArbitraryWrapping(other);
            }
        }
        forward_ref_op_assign! { impl BitXorAssign, bitxor_assign for ArbitraryWrapping<$t, $u>, $t }

        impl BitOr for ArbitraryWrapping<$t, $u> {
            type Output = ArbitraryWrapping<$t, $u>;

            #[inline]
            fn bitor(self, other: ArbitraryWrapping<$t, $u>) -> ArbitraryWrapping<$t, $u> {
                ArbitraryWrapping(wrap(self.0 | other.0, self.1, self.2), self.1, self.2)
            }
        }
        forward_ref_binop! { impl BitOr, bitor for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u> }

        impl BitOrAssign for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn bitor_assign(&mut self, other: ArbitraryWrapping<$t, $u>) {
                *self = *self | other;
            }
        }
        forward_ref_op_assign! { impl BitOrAssign, bitor_assign for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u> }

        impl BitOrAssign<$t> for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn bitor_assign(&mut self, other: $t) {
                *self = *self | ArbitraryWrapping(other);
            }
        }
        forward_ref_op_assign! { impl BitOrAssign, bitor_assign for ArbitraryWrapping<$t, $u>, $t }

        impl BitAnd for ArbitraryWrapping<$t, $u> {
            type Output = ArbitraryWrapping<$t, $u>;

            #[inline]
            fn bitand(self, other: ArbitraryWrapping<$t, $u>) -> ArbitraryWrapping<$t, $u> {
                ArbitraryWrapping(wrap(self.0 & other.0, self.1, self.2), self.1, self.2)
            }
        }
        forward_ref_binop! { impl BitAnd, bitand for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>}

        impl BitAndAssign for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn bitand_assign(&mut self, other: ArbitraryWrapping<$t, $u>) {
                *self = *self & other;
            }
        }
        forward_ref_op_assign! { impl BitAndAssign, bitand_assign for ArbitraryWrapping<$t, $u>, ArbitraryWrapping<$t, $u>}

        impl BitAndAssign<$t> for ArbitraryWrapping<$t, $u> {
            #[inline]
            fn bitand_assign(&mut self, other: $t) {
                *self = *self & ArbitraryWrapping(other);
            }
        }
        forward_ref_op_assign! { impl BitAndAssign, bitand_assign for ArbitraryWrapping<$t, $u>, $t }

        impl Neg for ArbitraryWrapping<$t, $u> {
            type Output = Self;
            #[inline]
            //TODO: panic if negation would be less than lower bound
            fn neg(self) -> Self {
                ArbitraryWrapping(0) - self
            }
        }
        forward_ref_unop! { impl Neg, neg for ArbitraryWrapping<$t, $u>}
    };
}

//wrapping_impl! { usize u8 u16 u32 u64 u128 isize i8 i16 i32 i64 i128 }

#[allow(dead_code, unused_macros)]
macro_rules! wrapping_int_impl {
    ($t:ident, $u:ident:ident, $f:ident) => {
        impl ArbitraryWrapping<$t, $u> {
            /// Returns the smallest value that can be represented by this integer type.
            ///
            /// # Examples
            ///
            /// Basic usage:
            ///
            /// ```
            /// #![feature(wrapping_int_impl)]
            /// use std::num::ArbitraryWrapping;
            ///
            #[doc = concat!("assert_eq!(<ArbitraryWrapping<", stringify!($t), ">>::MIN, ArbitraryWrapping(", stringify!($t), "::MIN));")]
            /// ```
            pub const MIN: $u = self.1;

            /// Returns the largest value that can be represented by this integer type.
            ///
            /// # Examples
            ///
            /// Basic usage:
            ///
            /// ```
            /// #![feature(wrapping_int_impl)]
            /// use std::num::ArbitraryWrapping;
            ///
            #[doc = concat!("assert_eq!(<ArbitraryWrapping<", stringify!($t), ">>::MAX, ArbitraryWrapping(", stringify!($t), "::MAX));")]
            /// ```
            pub const MAX: $v = self.2;

            /// Returns the size of this integer type in bits.
            ///
            /// # Examples
            ///
            /// Basic usage:
            ///
            /// ```
            /// #![feature(wrapping_int_impl)]
            /// use std::num::ArbitraryWrapping;
            ///
            #[doc = concat!("assert_eq!(<ArbitraryWrapping<", stringify!($t), ">>::BITS, ", stringify!($t), "::BITS);")]
            /// ```
            pub const BITS: u32 = <$t>::BITS;

            /// Returns the number of ones in the binary representation of `self`.
            ///
            /// # Examples
            ///
            /// Basic usage:
            ///
            /// ```
            /// #![feature(wrapping_int_impl)]
            /// use std::num::ArbitraryWrapping;
            ///
            #[doc = concat!("let n = ArbitraryWrapping(0b01001100", stringify!($t), ");")]
            ///
            /// assert_eq!(n.count_ones(), 3);
            /// ```
            #[inline]
            #[doc(alias = "popcount")]
            #[doc(alias = "popcnt")]
            #[must_use = "this returns the result of the operation, \
                          without modifying the original"]
            pub const fn count_ones(self) -> u32 {
                self.0.count_ones()
            }

            /// Returns the number of zeros in the binary representation of `self`.
            ///
            /// # Examples
            ///
            /// Basic usage:
            ///
            /// ```
            /// #![feature(wrapping_int_impl)]
            /// use std::num::ArbitraryWrapping;
            ///
            #[doc = concat!("assert_eq!(ArbitraryWrapping(!0", stringify!($t), ").count_zeros(), 0);")]
            /// ```
            #[inline]
            #[must_use = "this returns the result of the operation, \
                          without modifying the original"]
            pub const fn count_zeros(self) -> u32 {
                self.0.count_zeros()
            }

            /// Returns the number of trailing zeros in the binary representation of `self`.
            ///
            /// # Examples
            ///
            /// Basic usage:
            ///
            /// ```
            /// #![feature(wrapping_int_impl)]
            /// use std::num::ArbitraryWrapping;
            ///
            #[doc = concat!("let n = ArbitraryWrapping(0b0101000", stringify!($t), ");")]
            ///
            /// assert_eq!(n.trailing_zeros(), 3);
            /// ```
            #[inline]
            #[must_use = "this returns the result of the operation, \
                          without modifying the original"]
            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
            pub const fn trailing_zeros(self) -> u32 {
                self.0.trailing_zeros()
            }
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
//            /// Converts an integer from big endian to the target's endianness.
//            ///
//            /// On big endian this is a no-op. On little endian the bytes are
//            /// swapped.
//            ///
//            /// # Examples
//            ///
//            /// Basic usage:
//            ///
//            /// ```
//            /// #![feature(wrapping_int_impl)]
//            /// use std::num::ArbitraryWrapping;
//            ///
//            #[doc = concat!("let n = ArbitraryWrapping(0x1A", stringify!($t), ");")]
//            ///
//            /// if cfg!(target_endian = "big") {
//            #[doc = concat!("    assert_eq!(<ArbitraryWrapping<", stringify!($t), ">>::from_be(n), n)")]
//            /// } else {
//            #[doc = concat!("    assert_eq!(<ArbitraryWrapping<", stringify!($t), ">>::from_be(n), n.swap_bytes())")]
//            /// }
//            /// ```
//            #[inline]
//            #[must_use]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub const fn from_be(x: Self) -> Self {
//                ArbitraryWrapping(<$t>::from_be(x.0))
//            }
//
//            /// Converts an integer from little endian to the target's endianness.
//            ///
//            /// On little endian this is a no-op. On big endian the bytes are
//            /// swapped.
//            ///
//            /// # Examples
//            ///
//            /// Basic usage:
//            ///
//            /// ```
//            /// #![feature(wrapping_int_impl)]
//            /// use std::num::ArbitraryWrapping;
//            ///
//            #[doc = concat!("let n = ArbitraryWrapping(0x1A", stringify!($t), ");")]
//            ///
//            /// if cfg!(target_endian = "little") {
//            #[doc = concat!("    assert_eq!(<ArbitraryWrapping<", stringify!($t), ">>::from_le(n), n)")]
//            /// } else {
//            #[doc = concat!("    assert_eq!(<ArbitraryWrapping<", stringify!($t), ">>::from_le(n), n.swap_bytes())")]
//            /// }
//            /// ```
//            #[inline]
//            #[must_use]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub const fn from_le(x: Self) -> Self {
//                ArbitraryWrapping(<$t>::from_le(x.0))
//            }
//
//            /// Converts `self` to big endian from the target's endianness.
//            ///
//            /// On big endian this is a no-op. On little endian the bytes are
//            /// swapped.
//            ///
//            /// # Examples
//            ///
//            /// Basic usage:
//            ///
//            /// ```
//            /// #![feature(wrapping_int_impl)]
//            /// use std::num::ArbitraryWrapping;
//            ///
//            #[doc = concat!("let n = ArbitraryWrapping(0x1A", stringify!($t), ");")]
//            ///
//            /// if cfg!(target_endian = "big") {
//            ///     assert_eq!(n.to_be(), n)
//            /// } else {
//            ///     assert_eq!(n.to_be(), n.swap_bytes())
//            /// }
//            /// ```
//            #[inline]
//            #[must_use = "this returns the result of the operation, \
//                          without modifying the original"]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub const fn to_be(self) -> Self {
//                ArbitraryWrapping(self.0.to_be())
//            }
//
//            /// Converts `self` to little endian from the target's endianness.
//            ///
//            /// On little endian this is a no-op. On big endian the bytes are
//            /// swapped.
//            ///
//            /// # Examples
//            ///
//            /// Basic usage:
//            ///
//            /// ```
//            /// #![feature(wrapping_int_impl)]
//            /// use std::num::ArbitraryWrapping;
//            ///
//            #[doc = concat!("let n = ArbitraryWrapping(0x1A", stringify!($t), ");")]
//            ///
//            /// if cfg!(target_endian = "little") {
//            ///     assert_eq!(n.to_le(), n)
//            /// } else {
//            ///     assert_eq!(n.to_le(), n.swap_bytes())
//            /// }
//            /// ```
//            #[inline]
//            #[must_use = "this returns the result of the operation, \
//                          without modifying the original"]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub const fn to_le(self) -> Self {
//                ArbitraryWrapping(self.0.to_le())
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
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(3", stringify!($t), ").pow(4), ArbitraryWrapping(81));")]
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
    }
}
//
//wrapping_int_impl! { usize u8 u16 u32 u64 u128 isize i8 i16 i32 i64 i128 }
//
//macro_rules! wrapping_int_impl_signed {
//    ($($t:ty)*) => ($(
//        impl ArbitraryWrapping<$t, $u> {
//            /// Returns the number of leading zeros in the binary representation of `self`.
//            ///
//            /// # Examples
//            ///
//            /// Basic usage:
//            ///
//            /// ```
//            /// #![feature(wrapping_int_impl)]
//            /// use std::num::ArbitraryWrapping;
//            ///
//            #[doc = concat!("let n = ArbitraryWrapping(", stringify!($t), "::MAX) >> 2;")]
//            ///
//            /// assert_eq!(n.leading_zeros(), 3);
//            /// ```
//            #[inline]
//            #[must_use = "this returns the result of the operation, \
//                          without modifying the original"]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub const fn leading_zeros(self) -> u32 {
//                self.0.leading_zeros()
//            }
//
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
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(100", stringify!($t), ").abs(), ArbitraryWrapping(100));")]
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(-100", stringify!($t), ").abs(), ArbitraryWrapping(100));")]
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(", stringify!($t), "::MIN).abs(), ArbitraryWrapping(", stringify!($t), "::MIN));")]
//            /// assert_eq!(ArbitraryWrapping(-128i8).abs().0 as u8, 128u8);
//            /// ```
//            #[inline]
//            #[must_use = "this returns the result of the operation, \
//                          without modifying the original"]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub fn abs(self) -> ArbitraryWrapping<$t, $u> {
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
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(10", stringify!($t), ").signum(), ArbitraryWrapping(1));")]
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(0", stringify!($t), ").signum(), ArbitraryWrapping(0));")]
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(-10", stringify!($t), ").signum(), ArbitraryWrapping(-1));")]
//            /// ```
//            #[inline]
//            #[must_use = "this returns the result of the operation, \
//                          without modifying the original"]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub fn signum(self) -> ArbitraryWrapping<$t, $u> {
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
//            #[doc = concat!("assert!(ArbitraryWrapping(10", stringify!($t), ").is_positive());")]
//            #[doc = concat!("assert!(!ArbitraryWrapping(-10", stringify!($t), ").is_positive());")]
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
//            #[doc = concat!("assert!(ArbitraryWrapping(-10", stringify!($t), ").is_negative());")]
//            #[doc = concat!("assert!(!ArbitraryWrapping(10", stringify!($t), ").is_negative());")]
//            /// ```
//            #[must_use]
//            #[inline]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub const fn is_negative(self) -> bool {
//                self.0.is_negative()
//            }
//        }
//    )*)
//}
//
//wrapping_int_impl_signed! { isize i8 i16 i32 i64 i128 }

//macro_rules! wrapping_int_impl_unsigned {
//    ($($t:ty)*) => ($(
//        impl ArbitraryWrapping<$t, $u> {
//            /// Returns the number of leading zeros in the binary representation of `self`.
//            ///
//            /// # Examples
//            ///
//            /// Basic usage:
//            ///
//            /// ```
//            /// #![feature(wrapping_int_impl)]
//            /// use std::num::ArbitraryWrapping;
//            ///
//            #[doc = concat!("let n = ArbitraryWrapping(", stringify!($t), "::MAX) >> 2;")]
//            ///
//            /// assert_eq!(n.leading_zeros(), 2);
//            /// ```
//            #[inline]
//            #[must_use = "this returns the result of the operation, \
//                          without modifying the original"]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub const fn leading_zeros(self) -> u32 {
//                self.0.leading_zeros()
//            }
//
//            /// Returns `true` if and only if `self == 2^k` for some `k`.
//            ///
//            /// # Examples
//            ///
//            /// Basic usage:
//            ///
//            /// ```
//            /// #![feature(wrapping_int_impl)]
//            /// use std::num::ArbitraryWrapping;
//            ///
//            #[doc = concat!("assert!(ArbitraryWrapping(16", stringify!($t), ").is_power_of_two());")]
//            #[doc = concat!("assert!(!ArbitraryWrapping(10", stringify!($t), ").is_power_of_two());")]
//            /// ```
//            #[must_use]
//            #[inline]
//            #[unstable(feature = "wrapping_int_impl", issue = "32463")]
//            pub fn is_power_of_two(self) -> bool {
//                self.0.is_power_of_two()
//            }
//
//            /// Returns the smallest power of two greater than or equal to `self`.
//            ///
//            /// When return value overflows (i.e., `self > (1 << (N-1))` for type
//            /// `uN`), overflows to `2^N = 0`.
//            ///
//            /// # Examples
//            ///
//            /// Basic usage:
//            ///
//            /// ```
//            /// #![feature(wrapping_next_power_of_two)]
//            /// use std::num::ArbitraryWrapping;
//            ///
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(2", stringify!($t), ").next_power_of_two(), ArbitraryWrapping(2));")]
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(3", stringify!($t), ").next_power_of_two(), ArbitraryWrapping(4));")]
//            #[doc = concat!("assert_eq!(ArbitraryWrapping(200_u8).next_power_of_two(), ArbitraryWrapping(0));")]
//            /// ```
//            #[inline]
//            #[must_use = "this returns the result of the operation, \
//                          without modifying the original"]
//            #[unstable(feature = "wrapping_next_power_of_two", issue = "32463",
//                       reason = "needs decision on wrapping behaviour")]
//            pub fn next_power_of_two(self) -> Self {
//                ArbitraryWrapping(self.0.wrapping_next_power_of_two())
//            }
//        }
//    )*)
//}

//wrapping_int_impl_unsigned! { usize u8 u16 u32 u64 u128 }

//mod shift_max {
//    #![allow(non_upper_case_globals)]
//
//    #[cfg(target_pointer_width = "16")]
//    mod platform {
//        pub const usize: u32 = super::u16;
//        pub const isize: u32 = super::i16;
//    }
//
//    #[cfg(target_pointer_width = "32")]
//    mod platform {
//        pub const usize: u32 = super::u32;
//        pub const isize: u32 = super::i32;
//    }
//
//    #[cfg(target_pointer_width = "64")]
//    mod platform {
//        pub const usize: u32 = super::u64;
//        pub const isize: u32 = super::i64;
//    }
//
//    pub const i8: u32 = (1 << 3) - 1;
//    pub const i16: u32 = (1 << 4) - 1;
//    pub const i32: u32 = (1 << 5) - 1;
//    pub const i64: u32 = (1 << 6) - 1;
//    pub const i128: u32 = (1 << 7) - 1;
//    pub use self::platform::isize;
//
//    pub const u8: u32 = i8;
//    pub const u16: u32 = i16;
//    pub const u32: u32 = i32;
//    pub const u64: u32 = i64;
//    pub const u128: u32 = i128;
//    pub use self::platform::usize;
//}

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
