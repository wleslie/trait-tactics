//! Macros that provide common patterns for implementing traits in terms of other traits.
//!
//! # Binary operators
//!
//! A *binary operator trait* is a trait that has:
//! - A single generic type argument representing the right-hand-side operand, commonly defaulted to
//!   `Self`
//! - A single associated type `Output`
//! - A single method whose two arguments are `self` and the right-hand-side operand, returning
//!   `Self::Output`
//!
//! A *compound assignment operator trait* is a trait that has:
//! - A single generic type argument representing the right-hand-side operand, commonly defaulted to
//!   `Self`
//! - A single method whose two arguments are `&mut self` and the right-hand-side operand, returning
//!   `()`
//!
//! Particularly when overloading one of Rust's built-in binary operators, it is customary to
//! provide implementations not only for `A ⋄ B`, but also for `&A ⋄ B`, `A ⋄ &B`, and `&A ⋄ &B`
//! (where `⋄` stands for the operator to be overloaded, and `A` and `B` are operand types).
//! Furthermore, if the binary operator has a corresponding compound assignment operator (which we
//! will refer to as `⋄=`), it is customary to provide implements for `A ⋄= B` and `A ⋄= &B`. The
//! macros provided by this module assist in writing these additional trait implementations.
//!
//! First, implement `&A ⋄ &B`. This is the most general implementation, as all other
//! implementations can be written in terms of it.
//!
//! Then, provide the implementations suggested below, either by using the appropriate macro, or by
//! explicitly writing an optimized implementation.
//!
//! - If the binary operator has a corresponding compound assignment operator:
//!   1. Implement `A ⋄= &B` in terms of `&A ⋄ &B` using [assign_via_binop_ref_lhs].
//!   1. Implement `A ⋄= B` in terms of `A ⋄= &B` using [assign_via_assign_ref].
//!   1. Implement `A ⋄ &B` in terms of `A ⋄= &B` using [binop_via_assign].
//!   1. Implement `&A ⋄ B` in terms of `&A ⋄ &B` using [binop_via_binop_ref_rhs].
//!   1. Implement `A ⋄ B` in terms of `A ⋄= B` using [binop_via_assign].
//! - Otherwise:
//!   1. Implement `A ⋄ &B` in terms of `&A ⋄ &B` using [binop_via_binop_ref_lhs].
//!   1. Implement `&A ⋄ B` in terms of `&A ⋄ &B` using [binop_via_binop_ref_rhs].
//!   1. Implement `A ⋄ B` in one of two ways:
//!      - in terms of `&A ⋄ B` using [binop_via_binop_ref_lhs], *or*
//!      - in terms of `A ⋄ &B` using [binop_via_binop_ref_rhs] (preferred when `A ⋄ &B` is an
//!        optimized implementation).

/// Implements `A ⋄= B` in terms of `&A ⋄ &B` via `*self = &*self ⋄ y`.
///
/// See the [module documentation](self) for further information.
///
/// # Example
///
/// ```
/// # use std::ops::{Add, AddAssign};
/// # use trait_tactics::*;
/// struct S;
/// impl Add for &S {
///     type Output = S;
///     fn add(self, _rhs: Self) -> Self::Output { S }
/// }
///
/// #[assign_via_binop_ref_lhs(fn = add_assign, binop = Add::add)]
/// impl AddAssign<&S> for S {}
///
/// let mut x = S;
/// x += &S;
/// ```
pub use trait_tactics_macros::assign_via_binop_ref_lhs;

/// Implements `A ⋄= B` in terms of `A ⋄= &B` via `self ⋄= &y`.
///
/// See the [module documentation](self) for further information.
///
/// # Example
///
/// ```
/// # use std::ops::{Add, AddAssign};
/// # use trait_tactics::*;
/// struct S;
/// impl AddAssign<&S> for S {
///     fn add_assign(&mut self, _rhs: &S) {}
/// }
///
/// #[assign_via_assign_ref(fn = add_assign)]
/// impl AddAssign for S {}
///
/// let mut x = S;
/// x += S;
/// ```
pub use trait_tactics_macros::assign_via_assign_ref;

/// Implements `A ⋄ B` in terms of `A ⋄= B` via `x ⋄= y; x`.
///
/// As an "off-label use", this macro can also be used to implement `A ⋄ B` in terms of an arbitrary
/// function `f(&mut A, B)` via `f(&mut x, y); x`. `f` does not need to be a method of a compound
/// assignment trait.
///
/// See the [module documentation](self) for further information.
///
/// # Example
///
/// ```
/// # use std::ops::{Add, AddAssign};
/// # use trait_tactics::*;
/// struct S;
/// impl AddAssign for S {
///     fn add_assign(&mut self, _rhs: S) {}
/// }
///
/// #[binop_via_assign(fn = add, assign = AddAssign::add_assign)]
/// impl Add for S {}
///
/// S + S;
/// ```
pub use trait_tactics_macros::binop_via_assign;

/// Implements `A ⋄ B` in terms of `A ⋄ &B` via `x ⋄ &y`.
///
/// See the [module documentation](self) for further information.
///
/// # Example
///
/// ```
/// # use std::ops::Add;
/// # use trait_tactics::*;
/// struct S;
/// impl Add<&S> for S {
///     type Output = S;
///     fn add(self, _rhs: &S) -> Self::Output { S }
/// }
///
/// #[binop_via_binop_ref_rhs(fn = add, output = S)]
/// impl Add for S {}
///
/// S + S;
/// ```
pub use trait_tactics_macros::binop_via_binop_ref_rhs;

/// Implements `A ⋄ B` in terms of `&A ⋄ B` via `&x ⋄ y`.
///
/// See the [module documentation](self) for further information.
///
/// # Example
///
/// ```
/// # use std::ops::Add;
/// # use trait_tactics::*;
/// struct S;
/// impl Add<S> for &S {
///     type Output = S;
///     fn add(self, _rhs: S) -> Self::Output { S }
/// }
///
/// #[binop_via_binop_ref_lhs(fn = add, output = S)]
/// impl Add for S {}
///
/// S + S;
/// ```
pub use trait_tactics_macros::binop_via_binop_ref_lhs;

/// Implements [PartialOrd] in terms of [Ord].
///
/// # Example
///
/// ```
/// # use std::cmp::Ordering;
/// # use trait_tactics::*;
/// #[derive(PartialEq, Eq)]
/// struct S;
/// impl Ord for S {
///     fn cmp(&self, other: &Self) -> Ordering { Ordering::Equal }
/// }
///
/// #[partial_ord_via_ord]
/// impl PartialOrd for S {}
///
/// assert_eq!(S.partial_cmp(&S), Some(Ordering::Equal));
/// ```
pub use trait_tactics_macros::partial_ord_via_ord;

#[cfg(feature = "num-traits")]
/// Implements [Sum][std::iter::Sum] in terms of [Zero][num_traits::Zero] and [Add][std::ops::Add].
///
/// # Example
///
/// ```
/// #![no_implicit_prelude]
/// use ::std::{self, assert_eq, string::String, unimplemented, convert::From, iter::IntoIterator};
/// # use std::{iter::Sum, ops::Add};
/// # use ::num_traits::Zero;
/// # use trait_tactics::*;
/// #[derive(Debug, PartialEq, Eq)]
/// struct S(String);
/// impl Zero for S {
///     fn zero() -> Self { S(String::new()) }
///     fn is_zero(&self) -> bool { self.0.is_empty() }
/// }
/// impl Add for S {
///     type Output = Self;
///     fn add(mut self, rhs: Self) -> Self::Output {
///         self.0.push_str(&rhs.0);
///         self
///     }
/// }
///
/// #[sum_via_fold_zero_add]
/// impl Sum for S {}
///
/// let iter = [
///     S(String::from("foo")),
///     S(String::from("bar")),
///     S(String::from("baz")),
/// ].into_iter();
/// assert_eq!(std::iter::Iterator::sum::<S>(iter), S(String::from("foobarbaz")));
/// ```
pub use trait_tactics_macros::sum_via_fold_zero_add;
