use std::ops::{Deref, DerefMut};

use crate::params::{Num, GOAL};

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Vector([Num; GOAL.len()]);

impl Vector {
    pub fn constant(n: Num) -> Vector {
        Vector([n; GOAL.len()])
    }

    pub fn from_slice(ns: &[Num]) -> Vector {
        Vector(ns.try_into().expect("slice have same length"))
    }

    #[inline]
    pub fn map(mut self, function: impl Fn(Num) -> Num) -> Vector {
        for x in &mut self.0.iter_mut() {
            *x = function(*x)
        }
        self
    }
}

impl Deref for Vector {
    type Target = [Num; GOAL.len()];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Vector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

macro_rules! impl_op {
    ($trait:ident, $func:ident, $op:tt) => {
        impl std::ops::$trait<&Self> for Vector {
            type Output = Vector;

            fn $func(mut self, rhs: &Self) -> Self::Output {
                for (x, y) in self.iter_mut().zip(rhs.iter()) {
                    *x $op y;
                }
                self
            }
        }
    }
}

macro_rules! impl_unary {
    ($trait:ident, $func:ident, $op:tt) => {
        impl std::ops::$trait for Vector {
            type Output = Vector;

            fn $func(mut self) -> Self::Output {
                for x in self.iter_mut() {
                    *x = $op(*x);
                }
                self
            }
        }
    };
}

impl_op!(Add, add, +=);
impl_op!(Sub, sub, -=);
impl_op!(Mul, mul, *=);
impl_op!(Div, div, /=);
impl_op!(Rem, rem, %=);
impl_op!(BitAnd, bitand, &=);
impl_op!(BitOr, bitor, |=);
impl_op!(BitXor, bitxor, ^=);
impl_op!(Shl, shl, <<=);
impl_op!(Shr, shr, >>=);
impl_unary!(Not, not, !);
impl_unary!(Neg, neg, (|x| 0 - x));
