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
//!   1. Implement `A ⋄= &B` in terms of `&A ⋄ &B` using [assign_via_binop_ref_lhs!].
//!   1. Implement `A ⋄= B` in terms of `A ⋄= &B` using [assign_via_assign_ref!].
//!   1. Implement `A ⋄ &B` in terms of `A ⋄= &B` using [binop_via_assign!].
//!   1. Implement `&A ⋄ B` in terms of `&A ⋄ &B` using [binop_via_binop_ref_rhs!].
//!   1. Implement `A ⋄ B` in terms of `A ⋄= B` using [binop_via_assign!].
//! - Otherwise:
//!   1. Implement `A ⋄ &B` in terms of `&A ⋄ &B` using [binop_via_binop_ref_lhs!].
//!   1. Implement `&A ⋄ B` in terms of `&A ⋄ &B` using [binop_via_binop_ref_rhs!].
//!   1. Implement `A ⋄ B` in one of two ways:
//!      - in terms of `&A ⋄ B` using [binop_via_binop_ref_lhs!], *or*
//!      - in terms of `A ⋄ &B` using [binop_via_binop_ref_rhs!] (preferred when `A ⋄ &B` is an
//!        optimized implementation).

/// Implements `A ⋄= B` in terms of `&A ⋄ &B` via `*self = &*self ⋄ y`.
///
/// See the [module documentation](self) for further information.
pub use trait_tactics_macros::assign_via_binop_ref_lhs;

/// Implements `A ⋄= B` in terms of `A ⋄= &B` via `self ⋄= &y`.
///
/// See the [module documentation](self) for further information.
pub use trait_tactics_macros::assign_via_assign_ref;

/// Implements `A ⋄ B` in terms of `A ⋄= B` via `x ⋄= y; x`.
///
/// As an "off-label use", this macro can also be used to implement `A ⋄ B` in terms of an arbitrary
/// function `f(&mut A, B)` via `f(&mut x, y); x`. `f` does not need to be a method of a compound
/// assignment trait.
///
/// See the [module documentation](self) for further information.
pub use trait_tactics_macros::binop_via_assign;

/// Implements `A ⋄ B` in terms of `A ⋄ &B` via `x ⋄ &y`.
///
/// See the [module documentation](self) for further information.
pub use trait_tactics_macros::binop_via_binop_ref_rhs;

/// Implements `A ⋄ B` in terms of `&A ⋄ B` via `&x ⋄ y`.
///
/// See the [module documentation](self) for further information.
pub use trait_tactics_macros::binop_via_binop_ref_lhs;
