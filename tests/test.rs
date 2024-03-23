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

    assign_via_binop_ref_lhs!(impl<T, U> AddAssign<&B<U>> for A<T> where T: K { fn add_assign => Add::add });
    assign_via_assign_ref!(impl<T, U> AddAssign<B<U>> for A<T> where T: K { fn add_assign });
    binop_via_assign!(impl<T, U> Add<B<U>> for A<T> where T: K { fn add => AddAssign::add_assign });
    binop_via_assign!(impl<T, U> Add<&B<U>> for A<T> where T: K { fn add => AddAssign::add_assign });
    binop_via_binop_ref_rhs!(impl<T, U> Add<B<U>> for &A<T> where T: K { fn add -> A<T> });
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

    binop_via_binop_ref_lhs!(impl<T, U> Mul<&B<U>> for A<T> where T: K { fn mul -> A<T> });
    binop_via_binop_ref_rhs!(impl<T, U> Mul<B<U>> for &A<T> where T: K { fn mul -> A<T> });
    binop_via_binop_ref_rhs!(impl<T, U> Mul<B<U>> for A<T> where T: K { fn mul -> A<T> });
}
