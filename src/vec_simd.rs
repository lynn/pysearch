use std::{
    ops::{Deref, DerefMut, RangeBounds},
    simd::{i32x8, SimdPartialEq, SimdPartialOrd},
};

use crate::{
    gcd::gcd,
    params::{Num, C_STYLE_MOD, GOAL},
};

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Vector(i32x8);

impl Deref for Vector {
    type Target = i32x8;

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
                self.0 $op rhs.0;
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
                self.0 = $op(self.0);
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
impl_unary!(Neg, neg, -);

impl Vector {
    pub fn constant(n: Num) -> Vector {
        Vector(i32x8::splat(n))
    }

    pub fn from_slice(ns: &[Num]) -> Vector {
        let mut v = ns.to_owned();
        if v.len() > 8 {
            panic!("SIMD mode is limited to inputs of maximum size 8")
        } else if v.len() == 0 {
            panic!("Empty input")
        } else if v.len() < 8 {
            v = v.repeat(8);
            v.resize(8, 0);
        }
        Vector(i32x8::from_slice(&v))
    }
}

impl IntoIterator for Vector {
    type Item = Num;
    type IntoIter = std::array::IntoIter<Num, 8>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIterator::into_iter(self.0.to_array())
    }
}

pub fn divmod(left: &Vector, right: &Vector) -> Option<(Vector, Vector)> {
    if (right.simd_eq(i32x8::splat(0))
        | (left.simd_eq(i32x8::splat(Num::MIN)) & right.simd_eq(i32x8::splat(-1))))
    .any()
    {
        None
    } else if C_STYLE_MOD {
        Some((left.clone() / right, left.clone() % right))
    } else {
        let modulo = (left.clone() % right + right) % right;
        let div = (left.clone() - &modulo) / right;
        Some((div, modulo))
    }
}

pub fn vec_or(left: &Vector, right: &Vector) -> Vector {
    let left = left.clone();
    Vector(left.simd_eq(i32x8::splat(0)).select(**right, *left))
}

pub fn vec_eq(left: &Vector, right: &Vector) -> Vector {
    let left = left.clone();
    Vector(-left.simd_eq(right.0).to_int())
}

pub fn vec_lt(left: &Vector, right: &Vector) -> Vector {
    let left = left.clone();
    Vector(-left.simd_lt(right.0).to_int())
}

pub fn vec_le(left: &Vector, right: &Vector) -> Vector {
    let left = left.clone();
    Vector(-left.simd_le(right.0).to_int())
}

pub fn vec_gcd(left: &Vector, right: &Vector) -> Vector {
    let mut left = left.to_array();
    for (x, y) in left.iter_mut().zip(right.0.to_array()) {
        *x = gcd(*x, y);
    }
    Vector::from_slice(&left)
}

pub fn vec_pow(left: &Vector, right: &Vector) -> Vector {
    let mut left = left.to_array();
    for (x, y) in left.iter_mut().zip(right.0.to_array()) {
        *x = x.pow(y as u32);
    }
    Vector::from_slice(&left)
}

pub fn vec_in<R: RangeBounds<Num>>(vec: &Vector, bounds: R) -> bool {
    (0..GOAL.len()).all(|i| bounds.contains(&vec[i]))
}
