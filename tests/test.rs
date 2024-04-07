use std::ops::{Add, AddAssign, Mul};

use trait_tactics::*;

struct A<T>(T);
struct B<U>(U);

trait K {}

mod with_assign {
    use super::*;

    impl<T, U> Add<&B<U>> for &A<T>
    where
        T: K,
    {
        type Output = A<T>;
        fn add(self, _rhs: &B<U>) -> Self::Output {
            unimplemented!()
        }
    }

    #[assign_via_binop_ref_lhs(fn = add_assign, binop = Add::add)]
    impl<T, U> AddAssign<&B<U>> for A<T> where T: K {}
    #[assign_via_assign_ref(fn = add_assign)]
    impl<T, U> AddAssign<B<U>> for A<T> where T: K {}
    #[binop_via_assign(fn = add, assign = AddAssign::add_assign)]
    impl<T, U> Add<B<U>> for A<T> where T: K {}
    #[binop_via_assign(fn = add, assign = AddAssign::add_assign)]
    impl<T, U> Add<&B<U>> for A<T> where T: K {}
    #[binop_via_binop_ref_rhs(fn = add, output = A<T>)]
    impl<T, U> Add<B<U>> for &A<T> where T: K {}
}

mod without_assign {
    use super::*;

    impl<T, U> Mul<&B<U>> for &A<T>
    where
        T: K,
    {
        type Output = A<T>;
        fn mul(self, _rhs: &B<U>) -> Self::Output {
            unimplemented!()
        }
    }

    #[binop_via_binop_ref_rhs(fn = mul, output = A<T>)]
    impl<T, U> Mul<B<U>> for &A<T> where T: K {}
    #[binop_via_binop_ref_rhs(fn = mul, output = A<T>)]
    impl<T, U> Mul<B<U>> for A<T> where T: K {}
    #[binop_via_binop_ref_lhs(fn = mul, output = A<T>)]
    impl<T, U> Mul<&B<U>> for A<T> where T: K {}
}
