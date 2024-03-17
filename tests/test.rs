use std::ops::{Add, AddAssign};

use trait_tactics::*;

struct A<T>(T);
struct B<U>(U);

impl<T, U> Add<&B<U>> for &A<T>
where
    T: Clone,
{
    type Output = A<T>;
    fn add(self, _rhs: &B<U>) -> Self::Output {
        unimplemented!()
    }
}

assign_via_binop_ref! {
    impl<T, U> AddAssign<&B<U>> for A<T> where T: Clone { fn add_assign => Add::add }
}
assign_via_assign_ref! {
    impl<T, U> AddAssign<B<U>> for A<T> where T: Clone { fn add_assign }
}
